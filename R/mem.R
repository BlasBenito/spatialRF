#' @title Moran's Eigenvector Maps of a distance matrix
#' @description Computes the positive Moran's Eigenvector Maps of a distance matrix.
#' @usage
#' mem(
#'   x = NULL,
#'   distance.threshold = 0,
#'   colnames.prefix = "mem"
#' )
#' @param x Numeric distance matrix or data frame. Default: `NULL`.
#' @param distance.threshold Numeric vector with distance thresholds defining different neighborhood extents within the distance matrix, Default: 0
#' @param colnames.prefix Character, name prefix for the output columns. Default: `"mem"`
#' @return A data frame with positive Moran's Eigenvector Maps.
#' @details Takes the distance matrix `x`, double-centers it with [double_center_distance_matrix()], applies \link[base]{eigen}, and returns eigenvectors with positive normalized eigenvalues (a.k.a Moran's Eigenvector Maps, or MEMs). These MEMs are later used as spatial predictors by [rf_spatial()].
#' @seealso [mem_multithreshold()], [rf_spatial()]
#' @examples
#' \donttest{
#' if(interactive()){
#'
#'  data(distance_matrix)
#'
#'  distance_matrix.mem <- mem(x = distance_matrix)
#'  head(distance_matrix.mem)
#'
#' }
#' }
#' @rdname mem
#' @export
mem <- function(
  x = NULL,
  distance.threshold = 0,
  colnames.prefix = "mem"
  ){

  #double center distance matrix
  x <- double_center_distance_matrix(
    x = x,
    distance.threshold = distance.threshold
    )

  #computes eigenvectors
  mem <- eigen(x, symmetric = TRUE)

  #criteria to select mem
  mem.values.normalized <- mem$values/max(abs(mem$values))

  #get positive mem
  mem <- as.data.frame(mem$vectors[, which(mem.values.normalized > 0)])

  #adding colnames
  colnames(mem) <- paste(
    colnames.prefix,
    seq(1, ncol(mem)),
    sep = "_"
    )

  #returning output
  mem

}
