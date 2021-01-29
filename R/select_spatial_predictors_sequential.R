#' @title select_spatial_predictors_sequential
#' @description selects spatial predictors (either a distance matrix or the PCA factors of a distance matrix) by adding them sequentially to a model while monitoring the spatial correlation of the model residuals and the R-squared. Once all the available spatial predictors have been added to the model, the function identifies the first n predictors that minimize the spatial correlation of the residuals and maximize R-squared, and returns the names of the selected spatial predictors and a data frame with the selection criteria.
#' @param data (required) data frame with a response variable and a set of (preferably uncorrelated) predictors, Default: NULL
#' @param dependent.variable.name (required) string with the name of the response variable. Must be in the column names of 'data', Default: NULL
#' @param predictor.variable.names (required) character vector with the names of the predictive variables. Every element must be in the column names of 'data', Default: NULL
#' @param distance.matrix (optional) a squared matrix with the distances among the records in 'data'. Notice that the rows of 'distance.matrix' and 'data' must be the same. If not provided, the computation of the Moran's I of the residuals is ommited. Default: NULL.
#' @param distance.thresholds (optional) numeric vector, distances below each value in the distance matrix are set to 0 for the computation of Moran's I. If NULL, it defaults to seq(0, max(distance.matrix), length.out = 4). Default: NULL.
#' @param ranger.arguments list with \link[ranger]{ranger} arguments. See [rf] or [rf_repeat] for further details.
#' @param spatial.predictors.df data frame of spatial predictors, either a distance matrix, or the PCA factors of the distance matrix produced by [pca_multithreshold].
#' @param spatial.predictors.ranking ranking of predictors returned by [rank_spatial_predictors].
#' @param weight.r.squared numeric between 0 and 1, weight of R-squared in the optimization index.
#' @param weight.penalization.n.predictors numeric between 0 and 1, weight of the penalization for the number of spatial predictors added in the optimization index.
#' @param n.cores number of cores to use to compute repetitions. If NULL, all cores but one are used, unless a cluster is used.
#' @param cluster.ips character vector, IPs of the machines in the cluster. The first machine will be considered the main node of the cluster, and will generally be the machine on which the R code is being executed.
#' @param cluster.cores numeric integer vector, number of cores on each machine.
#' @param cluster.user character string, name of the user (should be the same throughout machines), Defaults to the current system user. Default: user name of the current session.
#' @param cluster.port integer, port used by the machines in the cluster to communicate. The firewall in all computers must allow traffic from and to such port. Default: 11000.
#' @return a list with two slots: `optimization`, a data frame with the index of the spatial predictor added on each iteration, the spatial correlation of the model residuals, and the R-squared of the model, and `best.spatial.predictors`, that is a character vector with the names of the spatial predictors that minimize the Moran's I of the residuals and maximize the R-squared of the model.
#' @details How does the algorithm work? If the function [rank_spatial_predictors] returns 10 spatial predictors (sp1 to sp10, ordered from best to worst), [select_spatial_predictors_sequential] is going to fit the models `y ~ predictors + sp1`, `y ~ predictors + sp1 + sp2`, until all spatial predictors are used in `y ~ predictors + sp1 ... sp10`. The model with lower Moran's I of the residuals and higher R-squared is selected, and its spatial predictors returned.
#' @examples
#' \dontrun{
#' if(interactive()){
#' data("distance_matrix")
#' data("plant_richness_df")
#'
#' #common arguments
#' data = plant_richness_df
#' dependent.variable.name = "richness_species_vascular"
#' predictor.variable.names = colnames(plant_richness_df)[5:21]
#' distance.matrix = distance_matrix
#' distance.thresholds = c(0, 100, 1000)
#'
#' #non-spatial model
#' model <- rf(
#'   data = data,
#'   dependent.variable.name = dependent.variable.name,
#'   predictor.variable.names = predictor.variable.names,
#'   distance.matrix = distance.matrix,
#'   distance.thresholds = distance.thresholds
#' )
#'
#' #preparing spatial predictors
#' spatial.predictors <- pca_distance_matrix(
#'   x = distance.matrix,
#'   distance.thresholds = distance.thresholds
#' )

#' #ranking spatial predictors by their Moran's I (faster option)
#' spatial.predictors.ranking <- rank_spatial_predictors(
#'   ranking.method = "mem",
#'   spatial.predictors.df = spatial.predictors,
#'   reference.moran.i = model$spatial.correlation.residuals$max.moran,
#'   distance.matrix = distance.matrix,
#'   distance.thresholds = distance.thresholds,
#'   n.cores = 1
#' )
#'
#' #selecting the best subset of predictors
#' selection <- select_spatial_predictors_sequential(
#'   data = data,
#'   dependent.variable.name = dependent.variable.name,
#'   predictor.variable.names = predictor.variable.names,
#'   distance.matrix = distance.matrix,
#'   distance.thresholds = distance.thresholds,
#'   spatial.predictors.df = spatial.predictors,
#'   spatial.predictors.ranking = spatial.predictors.ranking,
#'   n.cores = 1
#' )
#'
#' selection$optimization
#' selection$best.spatial.predictors
#'  }
#' }
#' @rdname select_spatial_predictors_sequential
#' @export
select_spatial_predictors_sequential <- function(
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  ranger.arguments = NULL,
  spatial.predictors.df = NULL,
  spatial.predictors.ranking = NULL,
  weight.r.squared = 0.75,
  weight.penalization.n.predictors = 0.25,
  n.cores = NULL,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = NULL,
  cluster.port = 11000
){

  #getting spatial.predictors.rank
  spatial.predictors.ranking <- spatial.predictors.ranking$ranking

  #weights limits
  if(weight.r.squared > 1){weight.r.squared <- 1}
  if(weight.r.squared < 0){weight.r.squared <- 0}
  if(weight.penalization.n.predictors > 1){weight.penalization.n.predictors <- 1}
  if(weight.penalization.n.predictors < 0){weight.penalization.n.predictors <- 0}

  #preparing fast ranger arguments
  if(is.null(ranger.arguments)){
    ranger.arguments <- list()
  }
  ranger.arguments$write.forest <- FALSE
  ranger.arguments$importance <- "none"
  ranger.arguments$local.importance <- FALSE
  ranger.arguments$keep.inbag <- FALSE
  ranger.arguments$write.forest <- FALSE
  ranger.arguments$num.trees <- 500
  ranger.arguments$data <- NULL
  ranger.arguments$formula <- NULL
  ranger.arguments$dependent.variable.name <- NULL
  ranger.arguments$predictor.variable.names <- NULL

  #preparing cluster for stand alone machine
  if(is.null(cluster.ips) == TRUE){

    #number of available cores
    if(is.null(n.cores)){
      n.cores <- parallel::detectCores() - 1
    }
    if(n.cores == 1){
      if(is.null(ranger.arguments)){
        ranger.arguments <- list()
      }
      ranger.arguments$num.threads <- 1
    }
    if(.Platform$OS.type == "windows"){
      temp.cluster <- parallel::makeCluster(
        n.cores,
        type = "PSOCK"
      )
    } else {
      temp.cluster <- parallel::makeCluster(
        n.cores,
        type = "FORK"
      )
    }

    #preparing beowulf cluster
  } else {

    #preparing the cluster specification
    cluster.spec <- cluster_specification(
      cluster.ips = cluster.ips,
      cluster.cores = cluster.cores,
      cluster.user = cluster.user
    )

    #setting parallel port
    Sys.setenv(R_PARALLEL_PORT = cluster.port)

    #cluster setup
    temp.cluster <- parallel::makeCluster(
      master = cluster.ips[1],
      spec = cluster.spec,
      port = Sys.getenv("R_PARALLEL_PORT"),
      outfile = "",
      homogeneous = TRUE
    )

  }
  doParallel::registerDoParallel(cl = temp.cluster)
  on.exit(parallel::stopCluster(cl = temp.cluster))

  spatial.predictors.i <- NULL
  optimization.df <- foreach::foreach(
    spatial.predictors.i = 1:length(spatial.predictors.ranking),
    .combine = "rbind",
    .packages = c(
      "ranger",
      "magrittr"
    ),
    .export = c(
      "root_mean_squared_error",
      "rescale_vector",
      "moran_multithreshold",
      "moran",
      "scale_robust"
    )
  ) %dopar% {

    #pca factor names
    spatial.predictors.selected.names.i <- spatial.predictors.ranking[1:spatial.predictors.i]

    #add pca factor to training data
    data.i <- data.frame(
      data,
      spatial.predictors.df[, spatial.predictors.selected.names.i]
    )
    colnames(data.i)[(ncol(data)+1):ncol(data.i)] <- spatial.predictors.selected.names.i

    #new predictor.variable.names
    predictor.variable.names.i <- c(
      predictor.variable.names,
      spatial.predictors.selected.names.i
    )

    #fitting model i
    m.i <- rf(
      data = data.i,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names.i,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments,
      seed = spatial.predictors.i,
      verbose = FALSE
    )

    #output.df
    out.df <- data.frame(
      spatial.predictor.index = spatial.predictors.i,
      moran.i = m.i$spatial.correlation.residuals$max.moran,
      p.value = m.i$spatial.correlation.residuals$per.distance[which.max(m.i$spatial.correlation.residuals$per.distance$moran.i), "p.value"],
      r.squared = m.i$r.squared
    )

    return(out.df)

  }#end of parallelized loop

  #preparing optimization df
  optimization.df <- data.frame(
    spatial.predictor.name = spatial.predictors.ranking,
    spatial.predictor.index = optimization.df$spatial.predictor.index,
    moran.i = optimization.df$moran.i,
    p.value = optimization.df$p.value,
    p.value.binary = ifelse(optimization.df$p.value >= 0.05, 1, 0),
    r.squared = optimization.df$r.squared,
    penalization.per.variable = (1/nrow(optimization.df)) * optimization.df$spatial.predictor.index
  )

  optimization.df$optimization <- optimization_function(
    x = optimization.df,
    weight.r.squared = weight.r.squared,
    weight.penalization.n.predictors = weight.penalization.n.predictors
  )

  #get index of spatial predictor with optimized r-squared and moran.i
  optimized.index <- which.max(optimization.df$optimization)

  #prepare vector with best factor names
  best.spatial.predictors <- spatial.predictors.ranking[1:optimized.index]

  #add column selected to optimization.df
  optimization.df$selected <- FALSE
  optimization.df[optimization.df$spatial.predictor.name %in% best.spatial.predictors, "selected"] <- TRUE

  #output list
  out.list <- list()
  out.list$optimization <- optimization.df
  out.list$best.spatial.predictors <- best.spatial.predictors

  #return output
  out.list

}
