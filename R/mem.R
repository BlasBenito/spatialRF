#' @title mem
#' @description Computes the positive Moran's Eigenvector Maps of a distance matrix.
#' @param x Numeric matrix or data frame, Default: NULL
#' @param distance.threshold distance.thresholds numeric vector with distance thresholds defining neighborhood in the distance matrix, Default: 0
#' @param colnames.prefix character, name prefix for the output columns. Default: 'mem'
#' @return A data frame with positive Moran's Eigenvector Maps
#' @details The function takes the distance matrix `x`, computes its weights and double-centers it with [double_center_distance_matrix], applies \link[base]{eigen}, and returns eigenvectors with positive normalized eigenvalues (eigenvalues/max(abs(eigenvalues)) > 0).
#' @examples
#' \dontrun{
#' if(interactive()){
#'  data(distance_matrix)
#'  distance_matrix.mem <- mem(x = distance_matrix)
#'  distance_matrix.mem
#'  }
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
  colnames(mem) <- paste(colnames.prefix, 1:ncol(mem), sep = "_")

  #returning output
  mem

}
