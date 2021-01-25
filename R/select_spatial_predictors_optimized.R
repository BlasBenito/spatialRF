#' @title select_spatial_predictors_optimized
#' @description selects spatial predictors (either a distance matrix or the PCA factors of a distance matrix) following these steps:
#' \enumerate{
#'   \item Gets the best spatial predictor yielded by [rank_spatial_predictors] and fits a model of the form `y ~ predictors + best_spatial_predictor_1`. The Moran's I of the residuals of this model are used as reference value for the next step.
#'   \item The remaining spatial predictors are introduced again into [rank_spatial_predictors], and the best one is introduced in a new model `y ~  predictors + best_spatial_predictor_1 + best_spatial_predictor_2`.
#'   \item Steps 1 and 2 are repeated until there are no more spatial predictors left.
#' }
#' This method allows to select the set of spatial predictors that have the largest joint effect in reducing the spatial correlation of the model residuals while maintaining an R-squared as high as possible. As a consequence of running [rank_spatial_predictors] on each iteration, this method includes in the final model less spatial predictors than [select_spatial_predictors_sequential] while minimizing spatial correlation and maximizing R-squared as much as possible.
#' @param data (required) data frame with a response variable and a set of (preferably uncorrelated) predictors, Default: NULL
#' @param dependent.variable.name (required) string with the name of the response variable. Must be in the column names of 'data', Default: NULL
#' @param predictor.variable.names (required) character vector with the names of the predictive variables. Every element must be in the column names of 'data', Default: NULL
#' @param distance.matrix (optional) a squared matrix with the distances among the records in 'data'. Notice that the rows of 'distance.matrix' and 'data' must be the same. If not provided, the computation of the Moran's I of the residuals is ommited. Default: NULL.
#' @param distance.thresholds (optional) numeric vector, distances below each value in the distance matrix are set to 0 for the computation of Moran's I. If NULL, it defaults to seq(0, max(distance.matrix), length.out = 4). Default: NULL.
#' @param ranger.arguments list with \link[ranger]{ranger} arguments. See [rf] or [rf_repeat] for further details.
#' @param spatial.predictors.df data frame of spatial predictors, either a distance matrix, or the PCA factors of the distance matrix produced by [pca_multithreshold].
#' @param spatial.predictors.ranking ranking of predictors returned by [rank_spatial_predictors].
#' @param n.cores number of cores to use to compute repetitions. If NULL, all cores but one are used, unless a cluster is used.
#' @param cluster.ips character vector, IPs of the machines in the cluster. The first machine will be considered the main node of the cluster, and will generally be the machine on which the R code is being executed.
#' @param cluster.cores numeric integer vector, number of cores on each machine.
#' @param cluster.user character string, name of the user (should be the same throughout machines), Defaults to the current system user. Default: user name of the current session.
#' @param cluster.port integer, port used by the machines in the cluster to communicate. The firewall in all computers must allow traffic from and to such port. Default: 11000.
#' @return a list with two slots: `optimization`, a data frame with the index of the spatial predictor added on each iteration, the spatial correlation of the model residuals, and the R-squared of the model, and `best.spatial.predictors`, that is a character vector with the names of the spatial predictors that minimize the Moran's I of the residuals and maximize the R-squared of the model.
#' @details How does the algorithm work? For example, if the function [rank_spatial_predictors] returns 10 ranked spatial predictors (sp1 to sp10, being sp7 the best one), [select_spatial_predictors_optimized] is going to first fit the model `y ~ predictors + sp7`. Then, the spatial predictors sp2 to sp9 are again ranked with [rank_spatial_predictors] using the model `y ~ predictors + sp7` as reference (at this stage, some of the spatial predictors might be dropped due to lack of effect). When the new ranking of spatial predictors is ready (let's say they are sp5, sp3, and sp4), the best one (sp5) is included in the model `y ~ predictors + sp7 + sp5`, and the remaining ones go again to [rank_spatial_predictors] to repeat the process until spatial predictors are depleted.
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
#'
#' #ranking spatial predictors
#' spatial.predictors.ranking <- rank_spatial_predictors(
#'   data = data,
#'   dependent.variable.name = dependent.variable.name,
#'   predictor.variable.names = predictor.variable.names,
#'   spatial.predictors.df = spatial.predictors,
#'   ranking.method = "moran.i.reduction",
#'   reference.moran.i = model$spatial.correlation.residuals$max.moran,
#'   distance.matrix = distance.matrix,
#'   distance.thresholds = distance.thresholds,
#'   n.cores = 1,
#'   multicollinearity.filter = "vif"
#' )
#'
#' #selecting the best subset of predictors
#' selection <- select_spatial_predictors_optimized(
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
#' @rdname select_spatial_predictors_optimized
#' @export
select_spatial_predictors_optimized <- function(
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  ranger.arguments = NULL,
  spatial.predictors.df = NULL,
  spatial.predictors.ranking = NULL,
  n.cores = NULL,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = NULL,
  cluster.port = 11000
){

  #initializing data for loop
  spatial.predictors.rank.i <- spatial.predictors.ranking
  spatial.predictors.candidates.i <- spatial.predictors.ranking$ranking

  #copy of data
  data.i <- data
  predictor.variable.names.i <- predictor.variable.names

  #vectors to build optimization.df
  optimization.index <- vector()
  optimization.spatial.predictors.name <- vector()
  optimization.moran.i <- vector()
  optimization.r.squared <- vector()
  optimization.sum <- vector()
  i <- 0

  #setting up default number of cores
  if(is.null(n.cores)){
    n.cores <- parallel::detectCores() - 1
  }

  #iterating
  while(length(spatial.predictors.candidates.i) > 1){

    i <- i + 1

    #subset spatial.predictors
    spatial.predictors.df.i <- spatial.predictors.df[, spatial.predictors.candidates.i, drop = FALSE]

    #add the first factor to data
    data.i <- data.frame(
      data.i,
      spatial.predictors.df[, spatial.predictors.candidates.i[1]]
    )
    colnames(data.i)[ncol(data.i)] <- spatial.predictors.candidates.i[1]


    #remove used column from spatial.predictors.df
    spatial.predictors.df.i <- spatial.predictors.df.i[, colnames(spatial.predictors.df.i) != spatial.predictors.candidates.i[1], drop = FALSE]

    #new predictor.variable.names
    predictor.variable.names.i <- c(
      predictor.variable.names.i,
      spatial.predictors.candidates.i[1]
    )

    #reference moran I
    reference.moran.i <- spatial.predictors.rank.i$ranking.criteria[spatial.predictors.rank.i$ranking.criteria$spatial.predictors.name == spatial.predictors.candidates.i[1], "model.moran.i"]

    #rank pca factors
    spatial.predictors.rank.i <- rank_spatial_predictors(
      data = data.i,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names.i,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments,
      spatial.predictors.df = spatial.predictors.df.i,
      ranking.method = "moran.i.reduction",
      reference.moran.i = reference.moran.i,
      multicollinearity.filter = "none",
      n.cores = n.cores,
      cluster.ips = cluster.ips,
      cluster.cores = cluster.cores,
      cluster.user = cluster.user,
      cluster.port = cluster.port
    )

    #redo pca.factors.candidates.i
    spatial.predictors.candidates.i <- spatial.predictors.rank.i$ranking

    #gathering data for optimization df.
    if(length(spatial.predictors.candidates.i) > 0){
      optimization.index[i] <- i
      optimization.spatial.predictors.name[i] <- spatial.predictors.rank.i$ranking[1]
      optimization.moran.i[i] <- spatial.predictors.rank.i$ranking.criteria[1, "model.moran.i"]
      optimization.r.squared[i] <- spatial.predictors.rank.i$ranking.criteria[1, "model.r.squared"]
      optimization.sum <- (1 - optimization.moran.i[i]) + optimization.r.squared[i]
    }



  }#end of while loop

  #putting together the optimization data frame
  optimization.df <- data.frame(
    spatial.predictors.index = optimization.index,
    spatial.predictors.name = optimization.spatial.predictors.name,
    moran.i = optimization.moran.i,
    r.squared = optimization.r.squared,
    sum = (1 - optimization.moran.i) + optimization.r.squared
  ) %>%
    dplyr::arrange(dplyr::desc(sum))

  #get index pca factor with optimized r-squared and moran.i
  optimized.index <- optimization.df[1, "spatial.predictors.index"]

  #prepare vector with best factor names
  best.spatial.predictors <- optimization.df[optimization.df$spatial.predictors.index %in% 1:optimized.index, "spatial.predictors.name"]

  #output list
  out.list <- list()
  out.list$optimization <- optimization.df
  out.list$best.spatial.predictors <- best.spatial.predictors

  #plot
  # x11()
  # par(mfrow = c(2, 1))
  # par(mar = c(1.5,4,2,2))
  # plot(
  #   optimization.df$spatial.predictors.index,
  #   optimization.df$r.squared,
  #   xlab = "",
  #   ylab = "R-squared",
  #   xaxt = 'n'
  # )
  # par(mar = c(4,4,0,2))
  # plot(
  #   optimization.df$spatial.predictors.index,
  #   optimization.df$moran.i,
  #   xlab = "PCA factors added",
  #   ylab = "Moran's I"
  # )

  #return output
  out.list

}
