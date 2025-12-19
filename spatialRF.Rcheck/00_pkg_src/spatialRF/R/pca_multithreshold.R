#' @title Compute Principal Component Analysis at multiple distance thresholds
#' @description Computes principal components of a distance matrix at multiple distance thresholds to generate multi-scale spatial predictors for [rf_spatial()]. Each distance threshold defines a different neighborhood scale, and PCA is applied to the weighted distance matrix at each scale.
#' @param distance.matrix Numeric distance matrix between observations.
#' @param distance.thresholds Numeric vector of distance thresholds defining different neighborhood scales. Each threshold specifies the maximum distance for spatial neighbors at that scale. If `NULL`, automatically computed with [default_distance_thresholds()]. Default: `NULL`.
#' @param max.spatial.predictors Integer specifying the maximum number of spatial predictors to retain. If the total number of generated predictors exceeds this value, only the first `max.spatial.predictors` are kept (ordered by variance explained). Useful for managing memory when `distance.matrix` is very large. Default: `NULL` (keeps all predictors).
#' @return Data frame where each column is a spatial predictor derived from PCA at a specific distance threshold. Columns are named with the pattern `spatial_predictor_<distance>_<number>` (e.g., "spatial_predictor_1000_1", "spatial_predictor_5000_2"), where `<distance>` is the distance threshold and `<number>` is the principal component rank. The number of rows matches the number of observations in `distance.matrix`.
#' @details
#' This function generates multi-scale spatial predictors by applying PCA to distance matrices at different neighborhood scales. The process for each distance threshold:
#' \enumerate{
#'   \item Converts the distance matrix to weights using [weights_from_distance_matrix()], where distances above the threshold are set to zero
#'   \item Applies [pca()] to the weighted distance matrix to extract principal components
#'   \item Names the resulting predictors with the distance threshold for identification
#'   \item Filters out predictors with all near-zero values
#' }
#'
#' **Multi-scale spatial modeling:**
#'
#' Different distance thresholds capture spatial patterns at different scales. Combining predictors from multiple thresholds allows [rf_spatial()] to account for spatial autocorrelation operating at multiple spatial scales simultaneously. This is analogous to [mem_multithreshold()] but uses PCA instead of Moran's Eigenvector Maps.
#'
#' **Comparison with MEMs:**
#'
#' Both [pca_multithreshold()] and [mem_multithreshold()] generate spatial predictors from distance matrices, but differ in their approach:
#' \itemize{
#'   \item **PCA**: Captures the main patterns of variation in the weighted distance matrix without considering spatial autocorrelation structure
#'   \item **MEMs**: Explicitly extracts spatial patterns with specific autocorrelation scales (positive and negative eigenvalues)
#' }
#'
#' In practice, MEMs are generally preferred for spatial modeling because they explicitly target spatial autocorrelation patterns, but PCA can serve as a simpler alternative or for comparison.
#' @seealso [pca()], [mem_multithreshold()], [weights_from_distance_matrix()], [default_distance_thresholds()]
#' @examples
#'
#' data(plants_distance)
#'
#' # Compute PCA spatial predictors at multiple distance thresholds
#' pca_predictors <- pca_multithreshold(
#'   distance.matrix = plants_distance,
#'   distance.thresholds = c(0, 1000, 5000)
#' )
#'
#' # View structure
#' head(pca_predictors)
#' dim(pca_predictors)
#'
#' # Check predictor names (show scale information)
#' colnames(pca_predictors)[1:6]
#'
#' # Limit number of predictors to save memory
#' pca_limited <- pca_multithreshold(
#'   distance.matrix = plants_distance,
#'   distance.thresholds = c(0, 1000, 5000),
#'   max.spatial.predictors = 20
#' )
#' ncol(pca_limited)  # At most 20 predictors
#'
#' @rdname pca_multithreshold
#' @family spatial_analysis
#' @export
pca_multithreshold <- function(
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

  #list to store pca factors
  pca.factors.list <- list()

  #iterating through distance thresholds
  for (distance.threshold.i in distance.thresholds) {
    #computing weighted distance matrix
    x.i <- weights_from_distance_matrix(
      distance.matrix = distance.matrix,
      distance.threshold = distance.threshold.i
    )

    #computing pca factors
    pca.factors.list[[as.character(distance.threshold.i)]] <- pca(
      x = x.i,
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
  names(pca.factors.list) <- NULL

  #to data frame
  pca.factors <- as.data.frame(do.call("cbind", pca.factors.list))

  #finding spatial predictors with too many leading zeroes
  pca.factors.filter <- round(colSums(abs(pca.factors)), 6)
  pca.factors.filter <- pca.factors.filter[pca.factors.filter != 0]

  #removing them
  pca.factors <- pca.factors[,
    colnames(pca.factors) %in% names(pca.factors.filter)
  ]

  #applying max.pca.factors
  if (!is.null(max.spatial.predictors)) {
    if (ncol(pca.factors) > max.spatial.predictors) {
      pca.factors <- pca.factors[, 1:max.spatial.predictors]
    }
  }

  #returning output
  pca.factors
}
