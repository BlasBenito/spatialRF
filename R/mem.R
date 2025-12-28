#' @title Compute Moran's Eigenvector Maps from distance matrix
#' @description Computes Moran's Eigenvector Maps (MEMs) from a distance matrix. Returns only eigenvectors with positive spatial autocorrelation, which capture broad to medium-scale spatial patterns.
#' @param distance.matrix Numeric distance matrix between spatial locations.
#' @param distance.threshold Numeric value specifying the maximum distance for spatial neighbors. Distances above this threshold are set to zero. Default: `0` (no thresholding).
#' @param colnames.prefix Character string used as prefix for column names in the output. Default: `"mem"`.
#' @return Data frame where each column is a MEM (spatial predictor) representing a different scale of spatial pattern. Columns are named with the pattern `<prefix>_<number>` (e.g., "mem_1", "mem_2").
#' @details
#' Moran's Eigenvector Maps (MEMs) are spatial variables that represent spatial structures at different scales. The function creates MEMs through the following steps:
#' \enumerate{
#'   \item Double-centers the distance matrix using [double_center_distance_matrix()]
#'   \item Computes eigenvectors and eigenvalues using [base::eigen()]
#'   \item Normalizes eigenvalues by dividing by the maximum absolute eigenvalue
#'   \item Selects only eigenvectors with positive normalized eigenvalues
#' }
#'
#' **Positive vs. negative eigenvalues:**
#'
#' Eigenvectors with positive eigenvalues represent positive spatial autocorrelation (nearby locations are similar), capturing broad to medium-scale spatial patterns. Eigenvectors with negative eigenvalues represent negative spatial autocorrelation (nearby locations are dissimilar) and are excluded. The returned MEMs are ordered by eigenvalue magnitude, with the first columns capturing the broadest spatial patterns.
#'
#' These MEMs are used as spatial predictors in [rf_spatial()] to account for spatial autocorrelation in model residuals.
#' @seealso [mem_multithreshold()], [rf_spatial()], [double_center_distance_matrix()]
#' @examples
#' data(plants_distance)
#'
#' # Compute MEMs from distance matrix
#' mems <- mem(distance.matrix = plants_distance)
#'
#' # View structure
#' head(mems)
#' dim(mems)
#'
#' # Check column names
#' colnames(mems)[1:5]
#'
#' @rdname mem
#' @family spatial_analysis
#' @export
#' @autoglobal
mem <- function(
  distance.matrix = NULL,
  distance.threshold = 0,
  colnames.prefix = "mem"
) {
  #stopping if no distance matrix
  if (is.null(distance.matrix)) {
    stop("The argument 'distance.matrix' is missing.")
  }

  #double center distance matrix
  distance.matrix.double.centered <- double_center_distance_matrix(
    distance.matrix = distance.matrix,
    distance.threshold = distance.threshold
  )

  #computes eigenvectors
  mem <- eigen(
    distance.matrix.double.centered,
    symmetric = TRUE
  )

  #normalize eigenvalues
  mem.values.normalized <- mem$values / max(abs(mem$values))

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
