#' @title PCA of a distance matrix over distance thresholds
#' @description Computes PCA factors of a distance matrix over different distance thresholds to generate spatial predictors for a model fitted with [rf_spatial()].
#' @param x Distance matrix with the same number of columns and rows. Default: `NULL`
#' @param distance.thresholds Numeric vector with distance thresholds defining neighborhood in the distance matrix, Default: `0`
#' @param max.spatial.predictors Integer, maximum number of spatial predictors to generate. Only useful when the distance matrix `x` is very large. Default: `NULL`
#' @return A data frame with the PCA factors of the thresholded matrix. The data frame columns are named "spatial_predictor_DISTANCE_COLUMN", where DISTANCE is the given distance threshold, and COLUMN is the column index of the given predictor.
#' @details The distance matrix is converted into weights with [weights_from_distance_matrix()] before computing the PCA. This produces more meaningful spatial predictors than using the distance matrix as is.
#' @seealso [pca()]
#' @examples
#' \donttest{
#' if(interactive()){
#'
#'  x <- pca_multithreshold(
#'    x = distance_matrix,
#'    distance.thresholds = c(0, 1000)
#'    )
#'  head(x)
#' }
#' }
#' @rdname pca_multithreshold
#' @export
pca_multithreshold <- function(
  x = NULL,
  distance.thresholds = 0,
  max.spatial.predictors = NULL
){

  #list to store pca factors
  pca.factors.list <- list()

  #iterating through distance thresholds
  for(distance.threshold.i in distance.thresholds){

    #computing weighted distance matrix
    x.i <- weights_from_distance_matrix(
      x = x,
      distance.threshold = distance.threshold.i
    )

    #computing pca factors
    pca.factors.list[[as.character(distance.threshold.i)]] <- pca(
      x = x.i,
      colnames.prefix = paste0(
        "spatial_predictor_",
        distance.threshold.i
      )
    )

  }

  #removing names
  names(pca.factors.list) <- NULL

  #to data frame
  pca.factors <- as.data.frame(do.call("cbind", pca.factors.list))

  #finding spatial predictors with too many leading zeroes
  pca.factors.filter <- round(apply(abs(pca.factors), 2, sum), 6)
  pca.factors.filter <- pca.factors.filter[pca.factors.filter != 0]

  #removing them
  pca.factors <- pca.factors[, colnames(pca.factors) %in% names(pca.factors.filter)]

  #applying max.pca.factors
  if(!is.null(max.spatial.predictors)){
    if(ncol(pca.factors) > max.spatial.predictors){
      pca.factors <- pca.factors[, 1:max.spatial.predictors]
    }
  }

  #returning output
  pca.factors

}
