#' @title Sequential introduction of spatial predictors into a model
#' @description Selects spatial predictors by adding them sequentially into a model while monitoring the Moran's I of the model residuals and the model's R-squared. Once all the available spatial predictors have been added to the model, the function identifies the first `n` predictors that minimize the spatial correlation of the residuals and maximize R-squared, and returns the names of the selected spatial predictors and a data frame with the selection criteria.
#' @param data Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param dependent.variable.name Character string with the name of the response variable. Must be in the column names of `data`. Default: `NULL`
#' @param predictor.variable.names Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param distance.matrix Squared matrix with the distances among the records in `data`. The number of rows of `distance.matrix` and `data` must be the same. If not provided, the computation of the Moran's I of the residuals is omitted. Default: `NULL`
#' @param distance.thresholds Numeric vector with neighborhood distances. All distances in the distance matrix below each value in `dustance.thresholds` are set to 0 for the computation of Moran's I. If `NULL`, it defaults to seq(0, max(distance.matrix), length.out = 4). Default: `NULL`
#' @param ranger.arguments Named list with \link[ranger]{ranger} arguments (other arguments of this function can also go here). All \link[ranger]{ranger} arguments are set to their default values except for 'importance', that is set to 'permutation' rather than 'none'. Please, consult the help file of \link[ranger]{ranger} if you are not familiar with the arguments of this function.
#' @param spatial.predictors.df Data frame of spatial predictors.
#' @param spatial.predictors.ranking Ranking of the spatial predictors returned by [rank_spatial_predictors()].
#' @param weight.r.squared Numeric between 0 and 1, weight of R-squared in the optimization index. Default: `0.75`
#' @param weight.penalization.n.predictors Numeric between 0 and 1, weight of the penalization for the number of spatial predictors added in the optimization index. Default: `0.25`
#' @param verbose Logical, ff `TRUE`, messages and plots generated during the execution of the function are displayed, Default: `FALSE`
#' @param cluster A cluster definition made with `parallel::makeCluster()`.
#' @param n.cores Integer, number of cores to use during computations. If `NULL`, all cores but one are used, unless a cluster is used. Default = `NULL`
#' @param cluster.ips Character vector with the IPs of the machines in a cluster. The machine with the first IP will be considered the main node of the cluster, and will generally be the machine on which the R code is being executed.
#' @param cluster.cores Numeric integer vector, number of cores to use on each machine.
#' @param cluster.user Character string, name of the user (should be the same throughout machines). Defaults to the current system user.
#' @param cluster.port Character, port used by the machines in the cluster to communicate. The firewall in all computers must allow traffic from and to such port. Default: `"11000"`
#' @return A list with two slots: `optimization`, a data frame with the index of the spatial predictor added on each iteration, the spatial correlation of the model residuals, and the R-squared of the model, and `best.spatial.predictors`, that is a character vector with the names of the spatial predictors that minimize the Moran's I of the residuals and maximize the R-squared of the model.
#' @details The algorithm works as follows: If the function [rank_spatial_predictors] returns 10 spatial predictors (sp1 to sp10, ordered from best to worst), [select_spatial_predictors_sequential] is going to fit the models `y ~ predictors + sp1`, `y ~ predictors + sp1 + sp2`, until all spatial predictors are used in `y ~ predictors + sp1 ... sp10`. The model with lower Moran's I of the residuals and higher R-squared (computed on the out-of-bag data) is selected, and its spatial predictors returned.
#' @examples
#' \donttest{
#' if(interactive()){
#'
#' data(distance_matrix)
#' data(plant_richness_df)
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
#' spatial.predictors <- mem_multithreshold(
#'   distance.matrix = distance.matrix,
#'   distance.thresholds = distance.thresholds
#' )

#' #ranking spatial predictors by their Moran's I (faster option)
#' spatial.predictors.ranking <- rank_spatial_predictors(
#'   ranking.method = "moran",
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
#' plot_optimization(selection$optimization)
#'
#' }
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
  verbose = FALSE,
  cluster = NULL,
  n.cores = NULL,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = Sys.info()[["user"]],
  cluster.port = "11000"
){

  #getting spatial.predictors.rank
  spatial.predictors.ranking <- spatial.predictors.ranking$ranking

  #weights limits
  if(is.null(weight.r.squared)){weight.r.squared <- 0.75}
  if(weight.r.squared > 1){weight.r.squared <- 1}
  if(weight.r.squared < 0){weight.r.squared <- 0}
  if(is.null(weight.penalization.n.predictors)){weight.penalization.n.predictors <- 0.25}
  if(weight.penalization.n.predictors > 1){weight.penalization.n.predictors <- 1}
  if(weight.penalization.n.predictors < 0){weight.penalization.n.predictors <- 0}

  #preparing fast ranger arguments
  if(is.null(ranger.arguments)){
    ranger.arguments <- list()
  }
  ranger.arguments$write.forest <- TRUE
  ranger.arguments$importance <- "none"
  ranger.arguments$local.importance <- FALSE
  ranger.arguments$keep.inbag <- FALSE
  ranger.arguments$num.trees <- 500
  ranger.arguments$data <- NULL
  ranger.arguments$formula <- NULL
  ranger.arguments$dependent.variable.name <- NULL
  ranger.arguments$predictor.variable.names <- NULL
  ranger.arguments$num.threads <- 1

  #if no cluster definition is provided
  if(is.null(cluster)){

    #setup of parallel execution
    if(is.null(n.cores)){

      n.cores <- parallel::detectCores() - 1
      `%dopar%` <- foreach::`%dopar%`

    } else {

      #only one core, no cluster
      if(n.cores == 1){

        #replaces dopar (parallel) by do (serial)
        `%dopar%` <- foreach::`%do%`
        on.exit(`%dopar%` <- foreach::`%dopar%`)

      } else {

        `%dopar%` <- foreach::`%dopar%`

      }

    }

    #local cluster
    if(is.null(cluster.ips) & n.cores > 1){

      temp.cluster <- parallel::makeCluster(
        n.cores,
        type = "PSOCK"
      )

      #register cluster and close on exit
      doParallel::registerDoParallel(cl = temp.cluster)

    }

    #beowulf cluster
    if(!is.null(cluster.ips)){


      #cluster port
      Sys.setenv(R_PARALLEL_PORT = cluster.port)

      #preparing the cluster specification
      cluster.spec <- cluster_specification(
        cluster.ips = cluster.ips,
        cluster.cores = cluster.cores,
        cluster.user = cluster.user
      )

      #cluster setup
      if(verbose == TRUE){
        outfile <- ""
      } else {
        if(.Platform$OS.type == "windows"){
          outfile <- "nul:"
        } else {
          outfile <- "/dev/null"
        }
      }
      temp.cluster <- parallel::makeCluster(
        master = cluster.ips[1],
        spec = cluster.spec,
        port = cluster.port,
        outfile = outfile,
        homogeneous = TRUE
      )

      #register cluster and close on exit
      doParallel::registerDoParallel(cl = temp.cluster)

    }

  } else {
    #registering cluster from the cluster argument
    doParallel::registerDoParallel(cl = cluster)
  }

  #parallelized loop
  spatial.predictors.i <- NULL
  optimization.df <- foreach::foreach(
    spatial.predictors.i = seq(1, length(spatial.predictors.ranking)),
    .combine = "rbind",
    .verbose = verbose
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
    m.i <- spatialRF::rf(
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
      p.value = m.i$spatial.correlation.residuals$per.distance[
        which.max(m.i$spatial.correlation.residuals$per.distance$moran.i),
        "p.value"
        ],
      r.squared = m.i$performance$r.squared.oob
    )

    return(out.df)

  }#end of parallelized loop

  #stopping cluster
  if(is.null(cluster) & n.cores > 1){
    parallel::stopCluster(cl = temp.cluster)
  }

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
