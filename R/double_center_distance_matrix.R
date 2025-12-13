#' @title Double centers a distance matrix
#' @description Generates a double-centered matrix (row and column means are zero) from the weights of a distance matrix (see [weights_from_plants_distance()]) and a distance threshold. This is a required step before the computation of Moran's Eigenvector Maps.
#' @usage
#' double_center_plants_distance (
#'   distance.matrix = NULL,
#'   distance.threshold = 0
#' )
#'
#'
#' @param distance.matrix Distance matrix. Default: `NULL`.
#' @param distance.threshold Numeric, positive, in the range of values of `x`. Distances below this value in the distance matrix are set to 0.  Default: `0`.
#' @return A double-centered matrix of the same dimensions as `x`.
#' @examples
#'
#'  #loading the distance matrix
#'  data(plants_distance)
#'
#'  x <- double_center_plants_distance(
#'    distance.matrix = plants_distance
#'  )
#'  x
#'
#' @seealso [weights_from_plants_distance()], [mem()], [mem_multithreshold()]
#' @rdname double_center_plants_distance
#' @export
double_center_plants_distance <- function(
  distance.matrix = NULL,
  distance.threshold = 0
) {
  if (is.null(distance.matrix)) {
    stop("Argument 'distance.matrix' is missing.`")
  }

  #distance matrix weights
  x <- weights_from_plants_distance(
    distance.matrix = distance.matrix,
    distance.threshold = distance.threshold
  )

  #bicenter matrix
  #compute row means
  x.row.means <- x * 0 + rowMeans(x)

  #compute col means
  x.col.means <- t(x * 0 + colMeans(x))

  #double centering
  x.double.centered <- (x - x.row.means - x.col.means + mean(x[]))

  #return output
  x.double.centered
}
