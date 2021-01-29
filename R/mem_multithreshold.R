#' @title mem_multithreshold
#' @description Computes Moran's Eigenvector Maps of a distance matrix (using [mem]) over different distance thresholds.
#' @param x x Numeric matrix or data frame, generally a distance matrix. Default: `NULL`.
#' @param distance.thresholds Numeric vector with distance thresholds defining neighborhood in the distance matrix, Default: 0.
#' @param max.spatial.predictors Maximum number of spatial predictors to generate. Only useful when the distance matrix *x* is very large. Default: 1000.
#' @return A data frame with as many rows as the matrix `x` with Moran's Eigenvector Maps. The data frame columns are named "spatial_predictor_DISTANCE_COLUMN", where distance is the given distance threshold, and column is the column index of the given predictor.
#' @details The function takes the distance matrix `x`, computes its weights at difference distance thresholds, double-centers the resulting weight matrices with [double_center_distance_matrix()], applies \link[base]{eigen} to each double-centered matrix, and returns eigenvectors with positive normalized eigenvalues for different distance thresholds.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  data(distance_matrix)
#'  x <- mem_multithreshold(
#'    x = distance_matrix,
#'    distance.thresholds = c(0, 1000, 2000)
#'    )
#'  }
#' }
#' @rdname mem_multithreshold
#' @export
mem_multithreshold <- function(
  x = NULL,
  distance.thresholds = 0,
  max.spatial.predictors = 1000
){

  #list to store mems
  mem.list <- list()

  #iterating through distance thresholds
  for(distance.threshold.i in distance.thresholds){

    mem.list[[as.character(distance.threshold.i)]] <- mem(
      x = x,
      distance.threshold = distance.threshold.i,
      colnames.prefix = paste0(
        "spatial_predictor_",
        distance.threshold.i
      )
    )

  }

  #removing names
  names(mem.list) <- NULL

  #to data frame
  mem.df <- as.data.frame(do.call("cbind", mem.list))

  #applying max.pca.factors
  if(ncol(mem.df) > max.spatial.predictors){
    mem.df <- mem.df[, 1:max.spatial.predictors]
  }

  #returning output
  mem.df

}
