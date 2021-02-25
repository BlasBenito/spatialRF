#' @title Finds optimal combinations of spatial predictors
#' @description Selects spatial predictors following these steps:
#' \enumerate{
#'   \item Gets the best spatial predictor yielded by [rank_spatial_predictors()] and fits a model of the form `y ~ predictors + best_spatial_predictor_1`. The Moran's I of the residuals of this model are used as reference value for the next step.
#'   \item The remaining spatial predictors are introduced again into [rank_spatial_predictors()], and the spatial predictor with the highest ranking is introduced in a new model of the form `y ~  predictors + best_spatial_predictor_1 + best_spatial_predictor_2`.
#'   \item Steps 1 and 2 are repeated until there are no more spatial predictors left.
#' }
#' This method allows to select the smallest set of spatial predictors that have the largest joint effect in reducing the spatial correlation of the model residuals, while maintaining the model's R-squared as high as possible. As a consequence of running [rank_spatial_predictors()] on each iteration, this method includes in the final model less spatial predictors than the sequential method implemented in [select_spatial_predictors_sequential()] would do, while minimizing spatial correlation and maximizing the R squared of the model as much as possible.
#' @param data Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param dependent.variable.name Character string with the name of the response variable. Must be in the column names of `data`. Default: `NULL`
#' @param predictor.variable.names Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param distance.matrix Squared matrix with the distances among the records in `data`. The number of rows of `distance.matrix` and `data` must be the same. If not provided, the computation of the Moran's I of the residuals is omitted. Default: `NULL`
#' @param distance.thresholds Numeric vector with neighborhood distances. All distances in the distance matrix below each value in `dustance.thresholds` are set to 0 for the computation of Moran's I. If `NULL`, it defaults to seq(0, max(distance.matrix), length.out = 4). Default: `NULL`
#' @param ranger.arguments Named list with \link[ranger]{ranger} arguments (other arguments of this function can also go here). All \link[ranger]{ranger} arguments are set to their default values except for 'importance', that is set to 'permutation' rather than 'none'. Please, consult the help file of \link[ranger]{ranger} if you are not familiar with the arguments of this function.
#' @param spatial.predictors.df Data frame of spatial predictors.
#' @param spatial.predictors.ranking Ranking of predictors returned by [rank_spatial_predictors()].
#' @param weight.r.squared Numeric between 0 and 1, weight of R-squared in the optimization index. Default: `0.25`
#' @param weight.penalization.n.predictors Numeric between 0 and 1, weight of the penalization for the number of spatial predictors added in the optimization index. Default: `0`
#' @param n.cores Integer, number of cores to use during computations. If `NULL`, all cores but one are used, unless a cluster is used. Default = `NULL`
#' @param cluster.ips Character vector with the IPs of the machines in a cluster. The machine with the first IP will be considered the main node of the cluster, and will generally be the machine on which the R code is being executed.
#' @param cluster.cores Numeric integer vector, number of cores to use on each machine.
#' @param cluster.user Character string, name of the user (should be the same throughout machines). Defaults to the current system user.
#' @param cluster.port Character, port used by the machines in the cluster to communicate. The firewall in all computers must allow traffic from and to such port. Default: `"11000"`
#' @return A list with two slots: `optimization`, a data frame with the index of the spatial predictor added on each iteration, the spatial correlation of the model residuals, and the R-squared of the model, and `best.spatial.predictors`, that is a character vector with the names of the spatial predictors that minimize the Moran's I of the residuals and maximize the R-squared of the model.
#' @details The algorithm works as follows. If the function [rank_spatial_predictors()] returns 10 ranked spatial predictors (sp1 to sp10, being sp7 the best one), [select_spatial_predictors_recursive()] is going to first fit the model `y ~ predictors + sp7`. Then, the spatial predictors sp2 to sp9 are again ranked with [rank_spatial_predictors()] using the model `y ~ predictors + sp7` as reference (at this stage, some of the spatial predictors might be dropped due to lack of effect). When the new ranking of spatial predictors is ready (let's say they are sp5, sp3, and sp4), the best one (sp5) is included in the model `y ~ predictors + sp7 + sp5`, and the remaining ones go again to [rank_spatial_predictors()] to repeat the process until spatial predictors are depleted.
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
#'   n.cores = 1
#' )
#'
#' #selecting the best subset of predictors
#' selection <- select_spatial_predictors_recursive(
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
#'
#' }
#' }
#' @rdname select_spatial_predictors_recursive
#' @export
select_spatial_predictors_recursive <- function(
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  ranger.arguments = NULL,
  spatial.predictors.df = NULL,
  spatial.predictors.ranking = NULL,
  weight.r.squared = 0.25,
  weight.penalization.n.predictors = 0,
  n.cores = NULL,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = Sys.info()[["user"]],
  cluster.port = "11000"
){

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

  #initializing data for loop
  spatial.predictors.ranking.i <- spatial.predictors.ranking
  spatial.predictors.candidates.i <- spatial.predictors.ranking$ranking

  #weights limits
  if(is.null(weight.r.squared)){weight.r.squared <- 0.25}
  if(weight.r.squared > 1){weight.r.squared <- 1}
  if(weight.r.squared < 0){weight.r.squared <- 0}
  if(is.null(weight.penalization.n.predictors)){weight.penalization.n.predictors <- 0}
  if(weight.penalization.n.predictors > 1){weight.penalization.n.predictors <- 1}
  if(weight.penalization.n.predictors < 0){weight.penalization.n.predictors <- 0}

  #copy of data
  data.i <- data
  predictor.variable.names.i <- predictor.variable.names

  #putting together the optimization data frame
  optimization.df <- data.frame(
    spatial.predictor.name = rep(NA, length(spatial.predictors.ranking$ranking)),
    spatial.predictor.index = rep(NA, length(spatial.predictors.ranking$ranking)),
    moran.i = rep(NA, length(spatial.predictors.ranking$ranking)),
    p.value = rep(NA, length(spatial.predictors.ranking$ranking)),
    p.value.binary  = rep(NA, length(spatial.predictors.ranking$ranking)),
    r.squared = rep(NA, length(spatial.predictors.ranking$ranking)),
    penalization.per.variable = rep(NA, length(spatial.predictors.ranking$ranking)),
    optimization = rep(NA, length(spatial.predictors.ranking$ranking))
  )

  #starting row counter
  i <- 0

  #vector to store the index of max(optimization.df$optimization)
  recursive.index.tracking <- vector()

  #iterating
  while(length(spatial.predictors.candidates.i) > 1){

    i <- i + 1

    #add the first factor to data
    data.i <- data.frame(
      data.i,
      spatial.predictors.df[, spatial.predictors.candidates.i[1]]
    )
    colnames(data.i)[ncol(data.i)] <- spatial.predictors.candidates.i[1]

    #new predictor.variable.names
    predictor.variable.names.i <- c(
      predictor.variable.names.i,
      spatial.predictors.candidates.i[1]
    )

    #reference moran I
    reference.moran.i <- spatial.predictors.ranking.i$criteria[spatial.predictors.ranking.i$criteria$spatial.predictors.name == spatial.predictors.candidates.i[1], "moran.i"]

    #subset and order spatial.predictors
    spatial.predictors.df.i <- spatial.predictors.df[, spatial.predictors.candidates.i[2:length(spatial.predictors.candidates.i)], drop = FALSE]

    #rank pca factors
    spatial.predictors.ranking.i <- rank_spatial_predictors(
      data = data.i,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names.i,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments,
      spatial.predictors.df = spatial.predictors.df.i,
      ranking.method = "effect",
      reference.moran.i = reference.moran.i,
      verbose = FALSE,
      n.cores = n.cores,
      cluster.ips = cluster.ips,
      cluster.cores = cluster.cores,
      cluster.user = cluster.user,
      cluster.port = cluster.port
    )

    #redo spatial.predictors.candidates.i
    spatial.predictors.candidates.i <- spatial.predictors.ranking.i$ranking

    #gathering data for optimization df.
    if(length(spatial.predictors.candidates.i) > 0){

      optimization.df[i, "spatial.predictor.index"] <- i
      optimization.df[i, "spatial.predictor.name"] <- spatial.predictors.ranking.i$ranking[1]
      optimization.df[i, "moran.i"] <- spatial.predictors.ranking.i$criteria[1, "moran.i"]
      optimization.df[i, "p.value"] <- spatial.predictors.ranking.i$criteria[1, "p.value"]
      optimization.df[i, "r.squared"] <- spatial.predictors.ranking.i$criteria[1, "model.r.squared"]
      optimization.df[i, "p.value.binary"] <- ifelse(optimization.df[i, "p.value"] >= 0.05, 1, 0)
      optimization.df[i, "penalization.per.variable"] <- (1/nrow(optimization.df)) * i
      optimization.df[i, "optimization"] <- (1 - optimization.df[i, "moran.i"]) + (weight.r.squared * optimization.df[i, "r.squared"]) - (weight.penalization.n.predictors * optimization.df[i, "penalization.per.variable"])

    }

    #getting the index with the maximum optimization
    recursive.index.tracking[i] <- optimization.df[which.max(optimization.df$optimization), "spatial.predictor.index"]

    #finding repetitions in the maximum value of recursive index
    if(sum(recursive.index.tracking == max(recursive.index.tracking)) > floor(nrow(optimization.df)/10)){
      break
    }

  }#end of while loop

  #remove empty rows
  optimization.df <- na.omit(optimization.df)

  #get index of spatial predictor with recursive r-squared and moran.i
  recursive.index <- which.max(optimization.df$optimization)

  #prepare vector with best factor names
  best.spatial.predictors <- optimization.df$spatial.predictor.name[1:recursive.index]

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
