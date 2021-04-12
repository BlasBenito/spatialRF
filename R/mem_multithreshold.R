#' @title Moran's Eigenvector Maps for different distance thresholds
#' @description Computes Moran's Eigenvector Maps of a distance matrix (using [mem()]) over different distance thresholds.
#' @param x x Numeric matrix or data frame, generally a distance matrix. Default: `NULL`.
#' @param distance.thresholds Numeric vector with distance thresholds defining neighborhood in the distance matrix, Default: `0`.
#' @param max.spatial.predictors Maximum number of spatial predictors to generate. Only useful to save memory when the distance matrix `x` is very large. Default: `NULL`.
#' @return A data frame with as many rows as the distance matrix `x` containing positive Moran's Eigenvector Maps. The data frame columns are named "spatial_predictor_DISTANCE_COLUMN", where DISTANCE is the given distance threshold, and COLUMN is the column index of the given spatial predictor.
#' @details The function takes the distance matrix `x`, computes its weights at difference distance thresholds, double-centers the resulting weight matrices with [double_center_distance_matrix()], applies \link[base]{eigen} to each double-centered matrix, and returns eigenvectors with positive normalized eigenvalues for different distance thresholds.
#' @examples
#' \donttest{
#' if(interactive()){
#'
#'  data(distance_matrix)
#'
#'  out <- mem_multithreshold(
#'    x = distance_matrix,
#'    distance.thresholds = c(0, 1000, 2000)
#'    )
#'  head(out)
#' }
#' }
#' @rdname mem_multithreshold
#' @export
mem_multithreshold <- function(
  x = NULL,
  distance.thresholds = NULL,
  max.spatial.predictors = NULL
){

  #creating distance thresholds
  if(is.null(distance.thresholds) == TRUE){
    distance.thresholds <- pretty(
      floor(
        seq(
          0,
          max(distance.matrix)/2,
          length.out = 4
        )
      )
    )
  }

  #list to store mems
  mem.list <- list()

  #iterating through distance thresholds
  for(distance.threshold.i in distance.thresholds){

    mem.list[[as.character(distance.threshold.i)]] <- mem(
      x = x,
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
  if(!is.null(max.spatial.predictors)){
    if(ncol(mem.df) > max.spatial.predictors){
      mem.df <- mem.df[, 1:max.spatial.predictors]
    }
  }

  #returning output
  mem.df

}
