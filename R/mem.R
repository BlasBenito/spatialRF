#' @title Moran's Eigenvector Maps of a distance matrix for one or several neighborhood distances
#' @description Computes the positive Moran's Eigenvector Maps of a distance matrix. These MEMs can be used as predictors in regression models to control for the spatial autocorrelation in the training data.
#' @param distance.matrix Distance matrix. Default: `NULL`.
#' @param distance.threshold (argument of [mem()]) Numeric vector with distance thresholds defining different neighborhood extents within the distance matrix, Default: `0`
#' @param distance.thresholds (argument of [mem_multithreshold()]) Numeric vector with distance thresholds defining neighborhood in the distance matrix, Default: `NULL`.
#' @param what.mem (optional, character) Character string indicating what Moran's Eigenvector Maps to generate. If "positive", only MEMs with positive eigenvectors are returned. If "negative", only MEMs with negative eigenvectors are returned. If "all", all MEMs are returned. Default: `"positive"`
#' @param colnames.prefix Character, name prefix for the output columns. Default: `"spatial_predictor_"`
#' @param max.spatial.predictors (argument of [mem_multithreshold()]) Positive integer, maximum number of Moran's Eigenvector Maps to return. Default: `NULL`.
#' @return A data frame with positive Moran's Eigenvector Maps.
#' @details Takes the distance matrix `x`, double-centers it with [double_center_distance_matrix()], applies \link[base]{eigen}, and returns eigenvectors with positive normalized eigenvalues (a.k.a Moran's Eigenvector Maps, or MEMs). These MEMs are later used as spatial predictors by [rf_spatial()].
#' @seealso [mem_multithreshold()], [rf_spatial()]
#' @examples
#' if(interactive()){
#'
#'  #loading example distance matrix
#'  data(ecoregions_distance_matrix)
#'
#'  #Moran's Eigenvector Maps of the distance matrix
#'  mem <- mem(x = ecoregions_distance_matrix)
#'
#'  #computing Moran's eigenvector maps for 0, 1000, and 2000 km
#'  mem.multithreshold <- mem_multithreshold(
#'    distance.matrix = ecoregions_distance_matrix,
#'    distance.thresholds = c(0, 1000, 2000)
#'    )
#'
#' }
#' @rdname mem
#' @export
mem <- function(
  distance.matrix = NULL,
  distance.threshold = 0,
  what.mem = "positive",
  colnames.prefix = "spatial_predictor_"
  ){

  #stopping if no distance matrix
  if(is.null(distance.matrix)){
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
  mem.values.normalized <- mem$values/max(abs(mem$values))

  #get positive mem
  if(is.null(what.mem)){
    what.mem <- "positive"
  }
  if(what.mem == "positive"){
    mem <- as.data.frame(mem$vectors[, which(mem.values.normalized > 0)])
  }
  if(what.mem == "negative"){
    mem <- as.data.frame(mem$vectors[, which(mem.values.normalized < 0)])
  }
  if(what.mem == "all"){
    mem <- as.data.frame(mem$vectors)
  }

  #adding colnames
  colnames(mem) <- paste(
    colnames.prefix,
    seq_len(ncol(mem)),
    sep = "_"
    )

  #returning output
  mem

}


#' @rdname mem
#' @export
mem_multithreshold <- function(
    distance.matrix = NULL,
    distance.thresholds = NULL,
    what.mem = "positive",
    colnames.prefix = "spatial_predictor_",
    max.spatial.predictors = NULL
){

  #stopping if no distance matrix
  if(is.null(distance.matrix)){
    stop("The argument 'distance.matrix' is missing.")
  }

  #creating distance thresholds
  if(is.null(distance.thresholds)){
    distance.thresholds <- default_distance_thresholds(distance.matrix = distance.matrix)
  } else {
    #removing distance thresholds larger than max(distance.matrix)
    distance.thresholds <- distance.thresholds[distance.thresholds < max(distance.matrix, na.rm = TRUE)]
  }

  #list to store mems
  mem.list <- list()

  #iterating through distance thresholds
  for(distance.threshold.i in distance.thresholds){

    mem.list[[as.character(distance.threshold.i)]] <- mem(
      distance.matrix = distance.matrix,
      distance.threshold = distance.threshold.i,
      what.mem = what.mem,
      colnames.prefix = paste0(
        colnames.prefix,
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

  #applying max.spatial.predictors
  if(!is.null(max.spatial.predictors)){
    if(ncol(mem.df) > max.spatial.predictors){
      mem.df <- mem.df[, 1:max.spatial.predictors]
    }
  }

  #returning output
  mem.df

}

