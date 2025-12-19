#' @title Double-center a distance matrix
#' @description Double-centers a distance matrix by converting it to weights and centering to zero row and column means. Required for computing Moran's Eigenvector Maps.
#' @param distance.matrix Numeric distance matrix. Default: `NULL`.
#' @param distance.threshold Numeric distance threshold for weight calculation. Distances above this threshold are set to 0 during weighting. Default: `0`.
#' @return Double-centered numeric matrix with the same dimensions as `distance.matrix`. The matrix has row means and column means of zero.
#' @details
#' Double-centering is performed in two steps:
#' \enumerate{
#'   \item Convert distances to weights using [weights_from_distance_matrix()]
#'   \item Center the matrix: subtract row means, subtract column means, and add the grand mean
#' }
#' The resulting matrix is symmetric with zero row and column means, suitable for Moran's Eigenvector Maps computation.
#' @examples
#' data(plants_distance)
#'
#' # Double-center the distance matrix
#' centered <- double_center_distance_matrix(
#'   distance.matrix = plants_distance
#' )
#'
#' # Verify row means are zero
#' head(rowMeans(centered))
#'
#' # Verify column means are zero
#' head(colMeans(centered))
#'
#' @seealso [weights_from_distance_matrix()], [mem()], [mem_multithreshold()]
#' @rdname double_center_distance_matrix
#' @family preprocessing
#' @export
double_center_distance_matrix <- function(
  distance.matrix = NULL,
  distance.threshold = 0
) {
  if (is.null(distance.matrix)) {
    stop("Argument 'distance.matrix' is missing.`")
  }

  #distance matrix weights
  x <- weights_from_distance_matrix(
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
