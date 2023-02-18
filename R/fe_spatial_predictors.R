#' Adds spatial predictors to a training data frame
#'
#' @description Adds spatial predictors to a training data frame.
#'
#' Spatial predictors represent the spatial structure of the training data, and are only required when model residuals show spatial autocorrelation. Spatial predictors are a proxy for spatial processes that might not be represented in other predictors.
#'
#' When using the method "hengl", the distance matrix is added as spatial component to the trainng data frame.
#'
#' When using the method "mem", then Moran's Eigenvector Maps (MEMs) are added as spatial component instead. Using the argument `what.mem` the user can select whether to add only positive, only negative, or all MEMs to the input data frame.
#'
#' @param data (required; data.frame or tibble) A data frame, tibble, or sf. Default: `NULL`.
#' @param response.name (optional; character string) Name of the dependent variable. Required when there are categorical variables in `predictors.names`. Default: `NULL`
#' @param predictors.names (required; character vector with column names of `data`) Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param distance.matrix (required; distance matrix) Squared matrix with the distances among the records in `data`. The number of rows of `distance.matrix` and `data` must be the same. Default: `NULL`
#' @param distance.thresholds  (optional; numeric vector with distances in the same units as `distance.matrix`) Numeric vector with neighborhood distances to be used to compute Moran's Eigenvector Maps. Ignored if `method = "hengl"`. Default: `NULL`
#' @param what.mem  (optional, character) Only relevant for `method = "mem`. Character string indicating what Moran's Eigenvector Maps to generate. If "positive", only MEMs with positive eigenvectors are returned. If "negative", only MEMs with negative eigenvectors are returned. If "all", all MEMs are returned. Default: `"positive"`
#' @param method (optional, character) Name of the method used to generate spatial predictors. If "hengl", the spatial predictors are the columns of the distance matrix between cases. If "mem", then Moran's Eigenvector Maps are used as spatial predictors.
#' @param cluster (optional, only relevant for `method = "mem"`; cluster object) A cluster definition generated with `parallel::makeCluster()` or \code{\link{start_cluster}}. Only advisable if you need to spread a large number of repetitions over the nodes of a large cluster when working with large data. The function does not stop a cluster, please remember to shut it down with `parallel::stopCluster(cl = cluster_name)` or `spatialRF::stop_cluster()` at the end of your pipeline. Default: `NULL`
#' @param verbose (optional, logical) If `TRUE`, the function prints message indicating what columns have been scaled. Default: `TRUE`
#' @return Adds new columns to the `data` argument. If `method = "hengl"`, then these columns are named "distance_from_X", where X is the index of the case the distance is being measured from. If `method = "mem"`, then these columns are named "mem_D_X", where D is the distance threshold for which the MEM was computed, and X is the index is the index of the case the MEM has been computed for.
#' @export
#'
#' @examples
#' if(interactive()){{
#'
#' data(
#'   ecoregions_df,
#'   ecoregions_distance_matrix,
#'   ecoregions_all_predictors,
#'   ecoregions_continuous_response
#'  )
#'
#'  #speed up MEM selection
#'  cluster <- start_cluster()
#'
#'  ecoregions_df <- fe_spatial_predictors(
#'    data = ecoregions_df,
#'    response.name = ecoregions_continuous_response,
#'    predictors.names = ecoregions_all_predictors,
#'    distance.matrix = ecoregions_distance_matrix,
#'    what.mem = "positive",
#'    method = "mem",
#'    cluster = cluster
#'    )
#'
#'    stop_cluster(cluster)
#'
#' }}
fe_spatial_predictors <- function(
    data = NULL,
    response.name = NULL,
    predictors.names = NULL,
    distance.matrix = NULL,
    distance.thresholds = NULL,
    what.mem = "positive",
    method = "mem",
    cluster = NULL,
    verbose = TRUE
    ){

  method <- match.arg(
    arg = method,
    choices = c(
      "mem",
      "hengl"
    ),
    several.ok = FALSE
  )

  #checking data
  ##############
  data <- check_data(
    data = data,
    verbose = verbose
  )

  predictors.names <- check_predictors_names(
    predictors.names = predictors.names,
    data = data,
    numeric.only = FALSE,
    is.required = TRUE,
    verbose = verbose
  )

  response.name <- check_response_name(
    response.name = response.name,
    data = data,
    is.required = FALSE,
    verbose = verbose
  )

  distance.matrix <- check_distance_matrix(
    data = data,
    distance.matrix = distance.matrix,
    is.required = TRUE,
    verbose = verbose
  )

  distance.thresholds <- check_distance_thresholds(
    distance.thresholds = distance.thresholds,
    distance.matrix = distance.matrix,
    is.required = TRUE,
    verbose = verbose
  )

  #hengl method
  if(method == "hengl"){

    #matrix to data frame
    rownames(distance.matrix) <- colnames(distance.matrix) <- paste0(
      "distance_from_",
      seq_len(ncol(distance.matrix))
    )

    #merging with data
    data <- cbind(
      data,
      as.data.frame(distance.matrix)
    )

  }

  #mem method
  if(method == "mem"){

    #generate mem
    mem.df <- mem_multithreshold(
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      what.mem = what.mem,
      colnames.prefix = "mem_"
    )

    #filter mem
    mem.df <- filter_spatial_predictors(
      data = data,
      predictors.names = predictors.names,
      response.name = response.name,
      spatial.predictors.df = mem.df,
      max.cor = 0.50
      )

    #rank mem (removes mem with neutral moran's i)
    mem.df.rank <- rank_spatial_predictors(
      spatial.predictors.df = mem.df,
      ranking.method = "moran",
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      n.cores = NULL,
      cluster = cluster,
      verbose = FALSE
    )

    #merging with data
    data <- cbind(
      data,
      mem.df.rank$spatial_predictors_df
    )


  }

  return(data)

}
