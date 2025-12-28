#' @title Compute Moran's Eigenvector Maps across multiple distance thresholds
#' @description Computes Moran's Eigenvector Maps (MEMs) using [mem()] at multiple distance thresholds and combines them into a single data frame. This creates spatial predictors capturing patterns at different spatial scales.
#' @param distance.matrix Numeric distance matrix between spatial locations.
#' @param distance.thresholds Numeric vector of distance thresholds. Each threshold defines the maximum distance for spatial neighbors at that scale. Default: `NULL` (automatically computed with [default_distance_thresholds()]).
#' @param max.spatial.predictors Integer specifying the maximum number of spatial predictors to return. If the total number of MEMs exceeds this value, only the first `max.spatial.predictors` columns are returned. Default: `NULL` (no limit).
#' @return Data frame with one row per observation (matching `distance.matrix` dimensions) and columns representing MEMs at different distance thresholds. Column names follow the pattern `spatial_predictor_<threshold>_<number>` (e.g., "spatial_predictor_0_1", "spatial_predictor_1000_2").
#' @details
#' This function generates spatial predictors at multiple spatial scales by computing MEMs at different distance thresholds. Different thresholds capture spatial patterns at different scales:
#' \itemize{
#'   \item Smaller thresholds (e.g., 0) capture fine-scale spatial patterns
#'   \item Larger thresholds capture broad-scale spatial patterns
#' }
#'
#' **Algorithm:**
#' \enumerate{
#'   \item For each distance threshold, calls [mem()] to compute MEMs
#'   \item Each [mem()] call applies the threshold, double-centers the matrix, and extracts positive eigenvectors
#'   \item Combines all MEMs into a single data frame
#'   \item Optionally limits the total number of predictors with `max.spatial.predictors`
#' }
#'
#' The resulting MEMs are used as spatial predictors in [rf_spatial()] to model spatial autocorrelation at multiple scales simultaneously.
#' @seealso [mem()], [rf_spatial()], [default_distance_thresholds()], [double_center_distance_matrix()]
#' @examples
#' data(plants_distance)
#'
#' # Compute MEMs for multiple distance thresholds
#' mems <- mem_multithreshold(
#'   distance.matrix = plants_distance,
#'   distance.thresholds = c(0, 1000, 5000)
#' )
#'
#' # View structure
#' head(mems)
#' dim(mems)
#'
#' # Check column names showing threshold and predictor number
#' colnames(mems)[1:6]
#'
#' # Limit number of spatial predictors
#' mems_limited <- mem_multithreshold(
#'   distance.matrix = plants_distance,
#'   distance.thresholds = c(0, 1000, 5000),
#'   max.spatial.predictors = 20
#' )
#' dim(mems_limited)
#'
#' @rdname mem_multithreshold
#' @family spatial_analysis
#' @export
#' @autoglobal
mem_multithreshold <- function(
  distance.matrix = NULL,
  distance.thresholds = NULL,
  max.spatial.predictors = NULL
) {
  #stopping if no distance matrix
  if (is.null(distance.matrix)) {
    stop("The argument 'distance.matrix' is missing.")
  }

  #creating distance thresholds
  if (is.null(distance.thresholds)) {
    distance.thresholds <- default_distance_thresholds(
      distance.matrix = distance.matrix
    )
  }

  #list to store mems
  mem.list <- list()

  #iterating through distance thresholds
  for (distance.threshold.i in distance.thresholds) {
    mem.list[[as.character(distance.threshold.i)]] <- mem(
      distance.matrix = distance.matrix,
      distance.threshold = distance.threshold.i,
      colnames.prefix = paste0(
        "spatial_predictor_",
        format(
          distance.threshold.i,
          scientific = FALSE
        )
      )
    )
  }

  #removing names
  names(mem.list) <- NULL

  #to data frame
  mem.df <- as.data.frame(do.call("cbind", mem.list))

  #applying max.pca.factors
  if (!is.null(max.spatial.predictors)) {
    if (ncol(mem.df) > max.spatial.predictors) {
      mem.df <- mem.df[, 1:max.spatial.predictors]
    }
  }

  #returning output
  mem.df
}
