#' @title Compute Principal Component Analysis
#' @description Computes principal components from a numeric matrix or data frame with automatic scaling and zero-variance removal. Returns all principal components as a data frame. Wrapper for [stats::prcomp()].
#' @param x Numeric matrix or data frame to decompose into principal components.
#' @param colnames.prefix Character string used as prefix for column names in the output. Default: `"pca_factor"`.
#' @return Data frame where each column is a principal component, ordered by decreasing variance explained. Columns are named with the pattern `<prefix>_<number>` (e.g., "pca_factor_1", "pca_factor_2"). The number of rows matches the number of rows in `x`.
#' @details
#' This function performs Principal Component Analysis (PCA) to create uncorrelated linear combinations of the original variables. The PCA process:
#' \enumerate{
#'   \item Removes columns with zero variance (constant values)
#'   \item Scales all remaining variables to mean = 0 and standard deviation = 1
#'   \item Computes principal components using singular value decomposition
#'   \item Returns all principal components ordered by decreasing variance explained
#' }
#'
#' **Usage in spatial analysis:**
#'
#' PCA is useful for dimension reduction when working with spatial distance matrices or multiple correlated spatial predictors. It creates orthogonal (uncorrelated) variables that capture the main patterns of variation while reducing dimensionality.
#'
#' For spatial modeling with [rf_spatial()], principal components of distance matrices can serve as alternative spatial predictors to Moran's Eigenvector Maps (MEMs). Use [pca_multithreshold()] to compute PCA across multiple distance thresholds for multi-scale spatial modeling.
#' @seealso [pca_multithreshold()], [mem()], [stats::prcomp()]
#' @examples
#' data(plants_distance)
#'
#' # Compute principal components from distance matrix
#' pca_components <- pca(x = plants_distance)
#'
#' # View structure
#' head(pca_components)
#' dim(pca_components)
#'
#' # Check column names
#' colnames(pca_components)[1:5]
#'
#' # Custom column prefix
#' pca_custom <- pca(
#'   x = plants_distance,
#'   colnames.prefix = "distance_pc"
#' )
#' colnames(pca_custom)[1:3]
#'
#' @rdname pca
#' @family spatial_analysis
#' @importFrom stats prcomp var
#' @export
pca <- function(
  x = NULL,
  colnames.prefix = "pca_factor"
) {
  if (inherits(x, "tbl_df") | inherits(x, "tbl")) {
    x <- as.data.frame(x)
  }

  #removing columns with zero variance
  x <- x[, which(apply(x, 2, var) != 0)]

  #computing pca of distance matrix
  x.pca <- prcomp(x, scale. = TRUE)

  #getting pca factors
  x.pca.factors <- as.data.frame(x.pca$x)
  colnames(x.pca.factors) <- paste(
    colnames.prefix,
    seq(1, ncol(x.pca.factors)),
    sep = "_"
  )

  #returning output
  x.pca.factors
}
