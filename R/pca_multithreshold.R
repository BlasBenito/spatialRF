#' @title pca_multithreshold
#' @description computes PCA factors of a distance matrix given a set of distance thresholds. This function allows to generate spatial predictors (Moran's Eigenvector Maps) from the same distance matrix thresholded at different neighborhood distances.
#' @param x (required) numeric squared matrix with distances among records, Default: NULL
#' @param distance.thresholds (optional) numeric vector with distance thresholds defining neighborhood in the distance matrix, Default: 0
#' @param max.spatial.predictors (optional) maximum number of spatial predictors to generate. Only useful when the distance matrix `x` is very large. Default: 1000
#' @return a data frame with the PCA factors of the thresholded matrix. The number of columns should be equal to ncol(x) * length(distance.thresholds), but notice that the function [pca] removes columns with variance 0, and therefore the final number of columns of the function output may vary. The data frame columns are named "spatial_predictor_DISTANCE_COLUMN", where distance is the given distance threshold, and column is the column index of the given predictor.
#' @details The distance matrix is converted into weights with [weights_from_distance_matrix] before computing the PCA. This produces more meaningful spatial predictors than using the distance matrix as is.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  x <- pca_multithreshold(
#'    x = distance_matrix,
#'    distance.thresholds = c(0, 1000)
#'    )
#'  x
#'  colnames(x)
#'  }
#' }
#' @rdname pca_multithreshold
#' @export
pca_multithreshold <- function(
  x = NULL,
  distance.thresholds = 0,
  max.spatial.predictors = 1000
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
  if(ncol(pca.factors) > max.spatial.predictors){
    pca.factors <- pca.factors[, 1:max.spatial.predictors]
  }

  #returning output
  pca.factors

}
