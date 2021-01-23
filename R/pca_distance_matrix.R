#' @title pca_distance_matrix
#' @description computes PCA factors of a distance matrix given a set of distance thresholds. This function allows to generate spatial predictors (Moran's Eigenvector Maps) from the same distance matrix thresholded at different neighborhood distances.
#' @param x numeric squared matrix with distances among records, Default: NULL
#' @param distance.thresholds numeric vector with distance thresholds defining neighborhood in the distance matrix, Default: 0
#' @return a data frame with the PCA factors of the thresholded matrix. The number of columns should be equal to ncol(x) * length(distance.thresholds), but notice that the function [pca] removes columns with variance 0, and therefore the final number of columns of the function output may vary. The columns are named "spatial_predictor_DISTANCE_COLUMN", where distance is the given distance threshold, and column is the column index of the given predictor.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  x <- pca_distance_matrix(
#'    x = distance_matrix,
#'    distance.thresholds = c(0, 1000)
#'    )
#'  x
#'  colnames(x)
#'  }
#' }
#' @rdname pca_distance_matrix
#' @export
pca_distance_matrix <- function(
  x = NULL,
  distance.thresholds = 0
){

  if(!is.matrix(x) | ncol(x) != nrow(x)){
    stop("x must be a squared matrix.")
  }

  #list to store pca factors
  pca.factors.list <- list()

  #iterating through distance thresholds
  for(distance.threshold.i in distance.thresholds){

    #copy distance matrix
    x.i <- x

    #applying threshold to distance matrix
    x.i[x.i <= distance.threshold.i] <- distance.threshold.i

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

  #returning output
  pca.factors

}
