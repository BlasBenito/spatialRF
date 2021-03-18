#' @title Fits spatial random forest models
#' @description Fits spatial random forest models using different methods to generate, rank, and select spatial predictors. The end goal is to provide the model with information about the spatial structure of the data in order to minimize the spatial correlation (Moran's I) of the model residuals.
#' @param model A model produced by [rf()] or [rf_repeat()]. If used, the arguments `data`, `dependent.variable.name`, `predictor.variable.names`, `distance.matrix`, `distance.thresholds`, `ranger.arguments`, and `scaled.importance` are taken directly from the model definition. Default: NULL
#' @param data Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param dependent.variable.name Character string with the name of the response variable. Must be in the column names of `data`. If the dependent variable is binary with values 1 and 0, the argument `case.weights` of `ranger` is populated by the function [case_weights()]. Default: `NULL`
#' @param predictor.variable.names Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param distance.matrix Squared matrix with the distances among the records in `data`. The number of rows of `distance.matrix` and `data` must be the same. If not provided, the computation of the Moran's I of the residuals is omitted. Default: `NULL`
#' @param distance.thresholds Numeric vector, distances below each value are set to 0 on separated copies of the distance matrix for the computation of Moran's I at different neighborhood distances. If `NULL`, it defaults to `seq(0, max(distance.matrix)/4, length.out = 2)`. Default: `NULL`
#' @param ranger.arguments Named list with \link[ranger]{ranger} arguments (other arguments of this function can also go here). All \link[ranger]{ranger} arguments are set to their default values except for 'importance', that is set to 'permutation' rather than 'none'. Please, consult the help file of \link[ranger]{ranger} if you are not familiar with the arguments of this function.
#' @param scaled.importance Logical. If `TRUE`, and 'importance = "permutation', the function scales 'data' with \link[base]{scale} and fits a new model to compute scaled variable importance scores. Default: `TRUE`
#' @param repetitions Integer, number of repetitions. If 1, [rf()] is used to fit the non-spatial and spatial models. If higher than one, [rf_repeat()] is used instead. Notice that using more than one repetition can get computationally costly if the selected method generated a large number of spatial predictors, as it is the case of the "hengl" method. Default: `1`
#' @param keep.models Logical. If `TRUE`, the fitted models are returned in the "models" slot. If `repetitions` is very high and `method` is "hengl",
#' setting `keep.models` to `TRUE` may cause memory issues. Default: `FALSE`
#' @param method Character, method to build, rank, and select spatial predictors. One of:
#' \itemize{
#'   \item "hengl"
#'   \item "hengl.moran.sequential"  (experimental)
#'   \item "hengl.effect.sequential" (experimental)
#'   \item "hengl.effect.recursive"  (experimental)
#'   \item "pca.moran.sequential"    (experimental)
#'   \item "pca.effect.sequential"   (experimental)
#'   \item "pca.effect.recursive"    (experimental)
#'   \item "mem.moran.sequential"
#'   \item "mem.effect.sequential"
#'   \item "mem.effect.recursive"
#' }
#' @param max.spatial.predictors Integer, maximum number of spatial predictors to generate. Useful when memory problems arise due to a large number of spatial predictors, Default: `NULL`
#' @param weight.r.squared Numeric between 0 and 1, weight of R-squared in the selection of spatial components. See Details, Default: `NULL`
#' @param weight.penalization.n.predictors Numeric between 0 and 1, weight of the penalization for adding an increasing number of spatial predictors during selection. Default: `NULL`
#' @param seed Integer, random seed to facilitate reproducibility. If set to a given number, the returned model is always the same. Only relevant if `repetitions = 1`. Default: `NULL`
#' @param verbose Logical. If TRUE, messages and plots generated during the execution of the function are displayed, Default: `TRUE`
#' @param n.cores Integer, number of cores to use during computations. If `NULL`, all cores but one are used, unless a cluster is used. Default = `NULL`
#' @param cluster.ips Character vector with the IPs of the machines in a cluster. The machine with the first IP will be considered the main node of the cluster, and will generally be the machine on which the R code is being executed.
#' @param cluster.cores Numeric integer vector, number of cores to use on each machine.
#' @param cluster.user Character string, name of the user (should be the same throughout machines). Defaults to the current system user.
#' @param cluster.port Character, port used by the machines in the cluster to communicate. The firewall in all computers must allow traffic from and to such port. Default: `"11000"`
#' @return A ranger model with several new slots:
#' \itemize{
#'   \item `ranger.arguments`: Values of the arguments used to fit the ranger model.
#'   \item `variable.importance`: A list containing the vector of variable importance as originally returned by ranger (scaled or not depending on the value of 'scaled.importance'), a data frame with the predictors ordered by their importance, and a ggplot showing the importance values.
#'   \item `performance`: With the out-of-bag R squared, pseudo R squared, RMSE and NRMSE of the model.
#'   \item `spatial.correlation.residuals`: Result of [moran_multithreshold()] applied in the model residuals.
#'   \item `selection.spatial.predictors`: A list with four slots:
#'   \itemize{
#'     \item `method`: Character, method used to generate, rank, and select spatial predictors.
#'     \item `names`: Character vector with the names of the selected spatial predictors. Not returned if the method is "hengl".
#'     \item `optimization`: Criteria used to select the spatial predictors. Not returned if the method is "hengl".
#'     \item `plot`: Plot of the criteria used to select the spatial predictors. Not returned if the method is "hengl".
#'   }
#' }
#' @details The function uses three different methods to generate spatial predictors ("hengl", "pca", and "mem"), two methods to rank them in order to define in what order they are introduced in the model ("effect" and "moran), and two methods to select the spatial predictors that minimize the spatial correlation of the model residuals ("sequential" and "recursive"). All method names but "hengl" (that uses the complete distance matrix as predictors in the spatial model) are named by combining a method to generate the spatial predictors, a method to rank them, and a method to select them, separated by a point. Examples are "mem.moran.sequential" or "mem.effect.recursive". All combinations are not possible, since the ranking method "moran" cannot be used with the selection method "recursive" (because the logics behind them are very different, see below).
#' Methods to generate spatial predictors:
#' \itemize{
#'   \item `"hengl"`: named after Tomislav Hengl and the paper [Hengl et al. (2018)](https://peerj.com/articles/5518/), where the authors propose to use the distance matrix among records as predictors in spatial random forest models (RFsp method). In this function, all methods starting with "hengl" use either the complete distance matrix, or select columns of the distance matrix as spatial predictors.
#'   \item `"mem"`: Generates Moran's Eigenvector Maps, that is, the eigenvectors of the double-centered weights of the distance matrix. The method is described in [Dray, Legendre and Peres-Neto (2006)](https://www.sciencedirect.com/science/article/abs/pii/S0304380006000925) and [Legendre and Gauthier (2014)](https://royalsocietypublishing.org/doi/10.1098/rspb.2013.2728).
#'   \item `"pca"`: Computes spatial predictors from the principal component analysis of a weighted distance matrix (see [weights_from_distance_matrix()]). This is an experimental method, use with caution.
#' }
#' Methods to rank spatial predictors (see [rank_spatial_predictors()]):
#' \itemize{
#'   \item `"moran"`: Computes the Moran's I of each spatial predictor, selects the ones with positive values, and ranks them from higher to lower Moran's I.
#'   \item `"effect"`: If a given non-spatial random forest model is defined as `y = p1 + ... + pn`, being `p1 + ... + pn` the set of predictors, for every spatial predictor generated (`spX`) a spatial model `y = p1 + ... + pn + spX` is fitted, and the Moran's I of its residuals is computed. The spatial predictors are then ranked by how much they help to reduce spatial autocorrelation between the non-spatial and the spatial model.
#' }
#' Methods to select spatial predictors:
#' \itemize{
#'   \item `"sequential"` (see [select_spatial_predictors_sequential()]): The spatial predictors are added one by one in the order they were ranked, and once all spatial predictors are introduced, the best first n predictors are selected. This method is similar to the one employed in the MEM methodology (Moran's Eigenvector Maps) described in [Dray, Legendre and Peres-Neto (2006)](https://www.sciencedirect.com/science/article/abs/pii/S0304380006000925) and [Legendre and Gauthier (2014)](https://royalsocietypublishing.org/doi/10.1098/rspb.2013.2728). This method generally introduces tens of predictors into the model, but usually offers good results.
#'   \item `"recursive"` (see [select_spatial_predictors_recursive()]): This method tries to find the smallest combination of spatial predictors that reduce the spatial correlation of the model's residuals the most. The algorithm goes as follows: 1. The first ranked spatial predictor is introduced into the model; 2. the remaining predictors are ranked again using the "effect" method, using the model in 1. as reference. The first spatial predictor in the resulting ranking is then introduced into the model, and the steps 1. and 2. are repeated until spatial predictors stop having an effect in reducing the Moran's I of the model residuals. This method takes longer to compute, but generates smaller sets of spatial predictors. This is an experimental method, use with caution.
#' }
#' Once ranking procedure is completed, an algorithm is used to select the minimal subset of spatial predictors that reduce the most the Moran's I of the residuals: for each new spatial predictor introduced in the model, the Moran's I of the residuals, it's p-value, a binary version of the p-value (0 if < 0.05 and 1 if >= 0.05), the R-squared of the model, and a penalization linear with the number of spatial predictors introduced (computed as `(1 / total spatial predictors) * introduced spatial predictors`) are rescaled between 0 and 1. Then, the optimization criteria is computed as `max(1 - Moran's I, p-value binary) + (weight.r.squared * R-squared) - (weight.penalization.n.predictors * penalization)`. The predictors from the first one to the one with the highest optimization criteria are then selected as the best ones in reducing the spatial correlation of the model residuals, and used along with `data` to fit the final spatial model.
#' @examples
#' \donttest{
#' if(interactive()){
#'
#'  data(distance_matrix)
#'  data(plant_richness_df)
#'  data <- plant_richness_df
#'  dependent.variable.name <- "richness_species_vascular"
#'  predictor.variable.names <- colnames(plant_richness_df)[5:21]
#'  distance.matrix <- distance_matrix
#'  distance.thresholds <- c(0, 500, 1000)
#'
#'  #hengl
#'  model <- rf_spatial(
#'    data = data,
#'    dependent.variable.name = dependent.variable.name,
#'    predictor.variable.names = predictor.variable.names,
#'    distance.matrix = distance.matrix,
#'    distance.thresholds = distance.thresholds,
#'    method = "hengl"
#'  )
#'
#'  #mem.moran.sequential
#'  model <- rf_spatial(
#'    data = data,
#'    dependent.variable.name = dependent.variable.name,
#'    predictor.variable.names = predictor.variable.names,
#'    distance.matrix = distance.matrix,
#'    distance.thresholds = distance.thresholds,
#'    method = "mem.moran.sequential",
#'    seed = 10
#'  )
#'
#'  #fitting an rf_spatial model from an rf model
#'  rf.model <- rf(
#'    data = plant_richness_df,
#'    dependent.variable.name = "richness_species_vascular",
#'    predictor.variable.names = colnames(plant_richness_df)[5:21],
#'    distance.matrix = distance_matrix,
#'    distance.thresholds = c(0, 1000, 2000)
#'  )
#'  rf.model$spatial.correlation.residuals$plot
#'
#'  rf.spatial <- rf_spatial(model = rf.model)
#'  rf.spatial$spatial.correlation.residuals$plot
#'
#'  #fitting an rf_spatial model from an rf_repeat model
#'  rf.repeat <- rf_repeat(
#'    data = plant_richness_df,
#'    dependent.variable.name = "richness_species_vascular",
#'    predictor.variable.names = colnames(plant_richness_df)[5:21],
#'    distance.matrix = distance_matrix,
#'    distance.thresholds = c(0, 1000, 2000),
#'    repetitions = 5
#'  )
#'  rf.repeat$spatial.correlation.residuals$plot
#'
#'  rf.spatial <- rf_spatial(model = rf.repeat)
#'  rf.spatial$spatial.correlation.residuals$plot
#'
#' }
#' }
#' @rdname rf_spatial
#' @export
rf_spatial <- function(
  model = NULL,
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  ranger.arguments = NULL,
  scaled.importance = TRUE,
  repetitions = 1,
  keep.models = FALSE,
  method = c(
    "mem.moran.sequential", #mem ordered by their Moran's I.
    "mem.effect.sequential", #mem added in order of effect.
    "mem.effect.recursive", #mem added maximizing their joint effect.
    "hengl", #all distance matrix columns as predictors
    "hengl.moran.sequential", #distance matrix columns added in the order of their Moran's I
    "hengl.effect.sequential", #distance matrix columns added in order of their effect.
    "hengl.effect.recursive", #distance matrix columns added maximizing their joint effect.
    "pca.moran.sequential", #pca factors added in order of their Moran's I.
    "pca.effect.sequential", #pca factors added in order of effect
    "pca.effect.recursive" #pca factors added maximizing their joint effect.
  ),
  max.spatial.predictors = NULL,
  weight.r.squared = NULL,
  weight.penalization.n.predictors = NULL,
  seed = NULL,
  verbose = TRUE,
  n.cores = NULL,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = Sys.info()[["user"]],
  cluster.port = "11000"
){

  #declaring variables
  moran.i <- NULL
  variable <- NULL

  #testing method argument
  method <- match.arg(
    arg = method,
    choices = c(
      "mem.moran.sequential",
      "mem.effect.sequential",
      "mem.effect.recursive",
      "hengl",
      "hengl.moran.sequential",
      "hengl.effect.sequential",
      "hengl.effect.recursive",
      "pca.moran.sequential",
      "pca.effect.sequential",
      "pca.effect.recursive"
    ),
    several.ok = FALSE
  )

  #getting arguments from model rather than ranger.arguments
  if(!is.null(model)){

    ranger.arguments <- model$ranger.arguments
    data <- ranger.arguments$data
    dependent.variable.name <- ranger.arguments$dependent.variable.name
    predictor.variable.names <- ranger.arguments$predictor.variable.names
    distance.matrix <- ranger.arguments$distance.matrix
    distance.thresholds <- ranger.arguments$distance.thresholds
    scaled.importance <- ranger.arguments$scaled.importance

    if(!is.null(ranger.arguments$repetitions)){
      repetitions <- ranger.arguments$repetitions
    }

    seed <- NULL
    importance <- "permutation"

  } else {

    #predictor.variable.names comes from auto_vif or auto_cor
    if(inherits(predictor.variable.names, "variable_selection")){
      predictor.variable.names <- predictor.variable.names$selected.variables
    }

  }

  #stopping if no distance matrix
  if(is.null(distance.matrix)){
    stop("The distance matrix is missing.")
  }

  if(is.null(distance.thresholds)){
    distance.thresholds <- floor(
      seq(
        0,
        max(distance.matrix)/4,
        length.out = 2
      )
    )
  }


  #GENERATING SPATIAL PREDICTORS
  #########################################################

  #HENGL
  if(method %in% c(
    "hengl",
    "hengl.moran.sequential",
    "hengl.effect.sequential",
    "hengl.effect.recursive"
  )
  ){

    #change name of distance matrix
    spatial.predictors.df <- distance.matrix
    colnames(spatial.predictors.df) <- rownames(spatial.predictors.df) <- paste0("spatial_predictor_", seq(1, ncol(distance.matrix)))

    if(verbose == TRUE){
      message("Using the distance matrix columns as spatial predictors.")
    }


  }

  #PCA
  if(method %in% c(
    "pca.moran.sequential",
    "pca.effect.sequential",
    "pca.effect.recursive"
  )
  ){

    #computing pca factors for pca methods
    spatial.predictors.df <- pca_multithreshold(
      x = distance.matrix,
      distance.thresholds =  distance.thresholds,
      max.spatial.predictors = max.spatial.predictors
    )

    if(verbose == TRUE){
      message("Using PCA factors of the distance matrix as spatial predictors.")
    }

  }

  #MEM
  if(method %in% c(
    "mem.moran.sequential",
    "mem.effect.sequential",
    "mem.effect.recursive"
  )
  ){

    #computing mem
    spatial.predictors.df <- mem_multithreshold(
      x = distance.matrix,
      distance.thresholds = distance.thresholds,
      max.spatial.predictors = max.spatial.predictors
    )

    if(verbose == TRUE){
      message("Using Moran's Eigenvector Maps as spatial predictors.")
    }

  }

  #selecting all column names of the distance matrix by default
  spatial.predictors.selected <- colnames(spatial.predictors.df)


  #RANKING SPATIAL PREDICTORS
  ###########################################################

  #SELECTING RANKING METHOD
  if(method %in% "hengl"){
    ranking.method <- NULL
  }

  if(method %in% c(
    "hengl.moran.sequential",
    "pca.moran.sequential",
    "mem.moran.sequential"
  )
  ){
    ranking.method <- "moran"
    if(verbose == TRUE){
      message("Ranking spatial predictors by their Moran's I.")
    }
  }

  if(method %in% c(
    "hengl.effect.sequential",
    "hengl.effect.recursive",
    "pca.effect.sequential",
    "pca.effect.recursive",
    "mem.effect.sequential",
    "mem.effect.recursive"
  )
  ){
    ranking.method <- "effect"
    if(verbose == TRUE){
      message("Ranking spatial predictors by how much they reduce Moran's I of the model residuals.")
    }
  }

  #RANKING SPATIAL PREDICTORS (if method is not "hengl")
  ######################################################
  if(!is.null(ranking.method)){

    #ranking spatial predictors
    spatial.predictors.ranking <- rank_spatial_predictors(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments,
      spatial.predictors.df = spatial.predictors.df,
      ranking.method = ranking.method,
      reference.moran.i = 1,
      verbose = FALSE,
      n.cores = n.cores,
      cluster.ips = cluster.ips,
      cluster.cores = cluster.cores,
      cluster.user = cluster.user,
      cluster.port = cluster.port
    )

  }

  #SELECTING SPATIAL PREDICTORS (if method is not "hengl")
  ######################################################

  #SEQUENTIAL SELECTION OF SPATIAL PREDICTORS
  if(method %in% c(
    "hengl.moran.sequential",
    "mem.moran.sequential",
    "pca.moran.sequential",
    "hengl.effect.sequential",
    "mem.effect.sequential",
    "pca.effect.sequential"
  )
  ){

    if(verbose == TRUE){
      message("Sequential selection of spatial predictors.")
    }

    #selecting spatial predictors sequentially
    spatial.predictors.selection <- select_spatial_predictors_sequential(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments,
      spatial.predictors.df = spatial.predictors.df,
      spatial.predictors.ranking = spatial.predictors.ranking,
      weight.r.squared = weight.r.squared,
      weight.penalization.n.predictors = weight.penalization.n.predictors,
      verbose = FALSE,
      n.cores = n.cores,
      cluster.ips = cluster.ips,
      cluster.cores = cluster.cores,
      cluster.user = cluster.user,
      cluster.port = cluster.port
    )

    #broadcast spatial.predictors.selected downstream
    spatial.predictors.selected <- as.character(spatial.predictors.selection$best.spatial.predictors)

  }


  #recursive SELECTION OF SPATIAL PREDICTORS
  if(method %in% c(
    "hengl.effect.recursive",
    "pca.effect.recursive",
    "mem.effect.recursive"
  )
  ){

    if(verbose == TRUE){
      message("Recursive selection of spatial predictors.")
    }

    #selecting spatial predictors by maximizing their joint effect
    spatial.predictors.selection <- select_spatial_predictors_recursive(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments,
      spatial.predictors.df = spatial.predictors.df,
      spatial.predictors.ranking = spatial.predictors.ranking,
      weight.r.squared = weight.r.squared,
      weight.penalization.n.predictors = weight.penalization.n.predictors,
      n.cores = n.cores,
      cluster.ips = cluster.ips,
      cluster.cores = cluster.cores,
      cluster.user = cluster.user,
      cluster.port = cluster.port
    )

    #broadcast spatial.predictors.selected
    spatial.predictors.selected <- spatial.predictors.selection$best.spatial.predictors

  }


  #FITTING SPATIAL MODEL
  ######################

  #subsetting spatial predictors
  spatial.predictors.df <- spatial.predictors.df[, spatial.predictors.selected]

  #prepare training data with best spatial predictors
  data.spatial <- data.frame(
    data,
    spatial.predictors.df
  )
  colnames(data.spatial) <- c(
    colnames(data),
    spatial.predictors.selected
  )

  #prepare predictor variable names
  predictor.variable.names.spatial <- c(
    predictor.variable.names,
    spatial.predictors.selected
  )

  #removing data from ranger arguments
  ranger.arguments$data <- NULL
  ranger.arguments$dependent.variable.name <- NULL
  ranger.arguments$predictor.variable.names <- NULL

  #fitting spatial model
  if(repetitions == 1){

    #fitting model
    model.spatial <- rf(
      data = data.spatial,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names.spatial,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments,
      seed = seed,
      verbose = FALSE
    )

    class(model.spatial) <- c("rf", "rf_spatial", "ranger")

  } else {

    model.spatial <- rf_repeat(
      data = data.spatial,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names.spatial,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments,
      scaled.importance = scaled.importance,
      repetitions = repetitions,
      keep.models = keep.models,
      verbose = FALSE,
      n.cores = n.cores,
      cluster.ips = cluster.ips,
      cluster.cores = cluster.cores,
      cluster.user = cluster.user,
      cluster.port = cluster.port
    )

    class(model.spatial) <- c("rf", "rf_spatial", "rf_repeat", "ranger")

  }

  #add moran's I plot
  model.spatial$spatial.correlation.residuals$plot <- plot_moran(
    x = model.spatial,
    verbose = FALSE
  )

  #preparing variable importance
  model.spatial$variable.importance <- prepare_importance_spatial(x = model.spatial)

  #adding spatial method and predictors to the model
  model.spatial$selection.spatial.predictors <- list()
  model.spatial$selection.spatial.predictors$method <- method
  model.spatial$selection.spatial.predictors$names <- spatial.predictors.selected
  if(exists("spatial.predictors.selection")){
    model.spatial$selection.spatial.predictors$optimization <- spatial.predictors.selection$optimization
    model.spatial$selection.spatial.predictors$plot <- plot_optimization(
      x = spatial.predictors.selection$optimization,
      verbose = FALSE
    )
  }

  if(verbose == TRUE){
    print(model.spatial)
    plot_importance(model.spatial)
    plot_moran(model.spatial)
  }

  #return output
  model.spatial

}
