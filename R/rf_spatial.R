#' @title rf_spatial
#' @description Fits spatial random forest models using different methods to generate, rank, and select spatial predictors. The end goal is to provide the model with information about the spatial structure of the data in order to minimize the spatial correlation of the residuals. See Details for a description of the methods.
#' @param model (optional) a model produced by [rf]. If used, the arguments `data`, `dependent.variable.name`, `predictor.variable.names`, `distance.matrix`, `distance.thresholds`, `ranger.arguments`, `trees.per.variable`, and `scaled.importance` are taken directly from the model definition. Default: NULL
#' @param data (required) data frame with a response variable and a set of (preferably uncorrelated) predictors, Default: NULL
#' @param dependent.variable.name (required) string with the name of the response variable. Must be in the column names of 'data', Default: NULL
#' @param predictor.variable.names (required) character vector with the names of the predictive variables. Every element must be in the column names of 'data', Default: NULL
#' @param distance.matrix (optional) a squared matrix with the distances among the records in 'data'. Notice that the rows of 'distance.matrix' and 'data' must be the same. If not provided, the computation of the Moran's I of the residuals is ommited. Default: NULL.
#' @param distance.thresholds (optional) numeric vector, distances below each value in the distance matrix are set to 0 for the computation of Moran's I. If NULL, it defaults to seq(0, max(distance.matrix), length.out = 4). Default: NULL.
#' @param ranger.arguments (optional) list with \link[ranger]{ranger} arguments. All \link[ranger]{ranger} arguments are set to their default values except for 'importance', that is set to 'permutation' rather than 'none'. Please, consult the help file of \link[ranger]{ranger} if you are not familiar with the arguments of this function.
#' @param trees.per.variable (optional) integer, number of individual regression trees to fit per variable in 'predictor.variable.names'. This is an alternative way to define ranger's 'num.trees'. If NULL, 'num.trees' is 500. Notice that for large numbers of spatial predictors using a high number of `trees.per.variable` can get computationally costly. Default: NULL
#' @param scaled.importance (optional) boolean. If TRUE, and 'importance = "permutation', the function scales 'data' with [scale_robust] and fits a new model to compute scaled variable importance scores. Default: TRUE
#' @param repetitions Integer, number of repetitions. If 1, [rf] is executed to fit the non-spatial and spatial models. If higher than one, [rf_repeat] is used instead. Notice that using more than one repetition can get computationally costly if the selected method generated a large number of spatial predictors, as it is the case of the "hengl" method. Default: 1
#' @param keep.models boolean, if TRUE, the fitted models are returned in the "models" slot (it sets `write.forest = TRUE` in the \link[ranger]{ranger} settings). If `repetitions` is very high and `method` is "hengl" this may cause memory issues. Default: FALSE.
#' @param method method to build, rank, and select spatial predictors. One of: "hengl", "hengl.moran.sequential", "hengl.effect.sequential", "hengl.effect.optimized", "pca.moran.sequential", "pca.effect.sequential", "pca.effect.optimized", "mem.moran.sequential", "mem.effect.sequential", "mem.effect.optimized". See details.
#' @param max.spatial.predictors integer, maximum number of spatial predictors to generate. Useful when memory problems arise due to a large number of records in `data`, Default: 1000
#' @param weight.r.squared numeric between 0 and 1, weight of R-squared in the selection of spatial components. See Details, Default: NULL
#' @param weight.penalization.n.predictors numeric between 0 and 1, weight of the penalization imposed with the addition of an increasing number of spatial predictors into a model, Default: NULL
#' @param scaled.importance (optional) boolean. If TRUE, and 'importance = "permutation', the function scales 'data' with [scale_robust] and fits a new model to compute scaled variable importance scores. Default: TRUE
#' @param seed (optional) integer, random seed to facilitate reproducibility. If set to a given number, the returned model is always the same. Only relevant if `repetitions = 1`. Default: NULL
#' @param repetitions integer, number of random forest models to fit. Default: 5
#' @param keep.models boolean, if TRUE, the fitted models are returned in the "models" slot. Default: FALSE.
#' @param verbose Boolean. If TRUE, messages and plots generated during the execution of the function are displayed, Default: TRUE
#' @param n.cores number of cores to use to compute repetitions. If NULL, all cores but one are used, unless a cluster is used.
#' @param cluster.ips character vector, IPs of the machines in the cluster. The first machine will be considered the main node of the cluster, and will generally be the machine on which the R code is being executed.
#' @param cluster.cores numeric integer vector, number of cores on each machine.
#' @param cluster.user character string, name of the user (should be the same throughout machines), Defaults to the current system user. Default: user name of the current session.
#' @param cluster.port integer, port used by the machines in the cluster to communicate. The firewall in all computers must allow traffic from and to such port. Default: 11000.
#' @return A ranger model with several new slots.
#' If `iterations = 1`, the slots generated by [rf] are returned:
#' \itemize{
#'   \item `ranger.arguments`: stores the values of the arguments used to fit the ranger model.
#'   \item `variable.importance`: a list containing the vector of variable importance as originally returned by ranger (scaled or not depending on the value of 'scaled.importance'), a data frame with the predictors ordered by their importance, and a ggplot showing the importance values.
#'   \item `pseudo.r.squared`: computed as the correlation between the observations and the predictions.
#'   \item `rmse`: as computed by [root_mean_squared_error] with 'normalization = NULL'.
#'   \item `nrmse`: as computed by [root_mean_squared_error] with 'normalization = "iq'.
#'   \item `residuals`: computed as observations minus predictions.
#'   \item `spatial.correlation.residuals`: the result of [moran_multithreshold].
#' }
#'  If `iterations > 1`, the slots generated by [rf_repeat] are returned:
#' \itemize{
#'   \item{ranger.arguments}{stores the values of the arguments used to fit the ranger model}
#'   \item{predictions}{a list with the predictions obtained on each repetition stored in a data frame named 'df.wide' and the average of the predictions in a data frame named 'df'}
#'   \item{variable.importance}{a list containing data frames with variable importance scores. These are `per.variable`, with the importance or average importance (if `repetitions > 1`) of each predictor, `per.repetition`, only if `repetitions > 1`, `spatial.predictors`, for plotting purposes, and `spatial.predictors.stats`, for printing purposes.}
#'   \item{pseudo.r.squared}{pseudo R-squared values throughout repetitions}
#'   \item{rmse}{rmse obtained on each repetition}
#'   \item{nrmse}{normalizad rmse obtained on each repetition}
#'   \item{residuals}{the residuals obtained on each repetition (df.wide), their mean (df) and their stats (stats)}
#'   \item{spatial.correlation.residuals}{the result of [moran_multithreshold] applied to the results of each repetition (df.long), the mean of Moran's I across repetitions (df), and a plot with the results of every repetition (plot)}
#' }
#' And in any case, two new slots are generated by [rf_spatial] if the original model needs spatial predictors to remove the spatial correlation of the residuals:
#' \itemize{
#'   \item performance.comparison:  A data frame with the R-squared, rmse, nrmse, and Moran's I of the residuals of the non-spatial and the spatial model.
#'   \item selection.spatial.predictors: a list with four slots:
#'   \itemize{
#'     \item method string, method used to generate, rank, and select spatial predictors.
#'     \item names character vector with the names of the selected spatial predictors. Not returned if the method is "hengl".
#'     \item df criteria used to select the spatial predictors. Not returned if the method is "hengl".
#'     \item plot plot of the criteria used to select the spatial predictors. Not returned if the method is "hengl".
#'   }
#' }
#' @details The function uses three different methods to generate spatial predictors ("hengl", "pca", and "mem"), two methods to rank them in order to define in what order they are introduced in the model ("effect" and "moran), and two methods to select the spatial predictors that minimize the spatial correlation of the model residuals ("sequential" and "optimized"). All method names but "hengl" (that uses the complete distance matrix as predictors in the spatial model) are named by combining a method to generate the spatial predictors, a method to rank them, and a method to select them, separated by a point. Examples are "mem.moran.sequential" or "mem.effect.optimized". All combinations are not possible, since the ranking method "moran" cannot be used with the selection method "optimized" (because the logics behind them are very different, see below).
#' Methods to generate spatial predictors:
#' \itemize{
#'   \item hengl: named after Tomislav Hengl and the paper [Hengl et al. (2018)](https://peerj.com/articles/5518/), where the authors propose to use the distance matrix among records as a set of covariates in spatial random forest models (RFsp method). In this function, all methods starting with "hengl" use either the complete distance matrix, or select columns of the distance matrix.
#'   \item mem: generates Moran's Eigenvector Maps, that is, the eigenvectors of the double-centered weights of the distance matrix. The method is described in [Dray, Legendre and Peres-Neto (2006)](https://www.sciencedirect.com/science/article/abs/pii/S0304380006000925) and [Legendre and Gauthier (2014)](https://royalsocietypublishing.org/doi/10.1098/rspb.2013.2728).
#'   \item pca: computes spatial predictors from the principal component analysis of a weighted distance matrix (see [weights_from_distance_matrix]). This method to generate spatial predictors is the one with the least performance in the preliminary trials of this function.
#' }
#' Methods to rank spatial predictors (see [rank_spatial_predictors]):
#' \itemize{
#'   \item moran: Computes the Moran's I of each spatial predictor, selects the ones with positive values, and ranks them from higher to lower Moran's I.
#'   \item effect: If a given non-spatial random forest model is defined as `y = p1 + ... + pn`, being `p1 + ... + pn` the set of predictors, for every spatial predictor generated (`spX`) a spatial model `y = p1 + ... + pn + spX` is fitted, and the Moran's I of its residuals is computed. The spatial predictors are then ranked by how much they help to reduce spatial autocorrelation between the non-spatial and the spatial model.
#' }
#' Methods to select spatial predictors:
#' \itemize{
#'   \item sequential (see [select_spatial_predictors_sequential]): The spatial predictors are added one by one in the order they were ranked, and once all spatial predictors are introduced, the best first n predictors are selected. This method is similar to the one employed in the MEM methodology (Moran's Eigenvector Maps) described in [Dray, Legendre and Peres-Neto (2006)](https://www.sciencedirect.com/science/article/abs/pii/S0304380006000925) and [Legendre and Gauthier (2014)](https://royalsocietypublishing.org/doi/10.1098/rspb.2013.2728). This method generally introduces tens of predictors into the model.
#'   \item optimized (see [select_spatial_predictors_optimized]): This method tries to find the smallest combination of spatial predictors that reduce the spatial correlation of the model's residuals the most. The algorithm goes as follows: 1. The first ranked spatial predictor is introduced into the model; 2. the remaining predictors are ranked again using the "effect" method, using the model in 1. as reference. The first spatial predictor in the resulting ranking is then introduced into the model, and the steps 1. and 2. are repeated until spatial predictors stop having an effect in reducing the Moran's I of the model residuals. This method takes longer to compute, but generates smaller sets of spatial predictors.
#' }
#' Once a selection procedure is comlete, an algorithm is used to select the best subset of spatial predictors: for each new predictor introduced, the Moran's I of the residuals, it's p-value, a binary version of the p-value (0 if < 0.05 and 1 if >= 0.05), the R-squared of the model, and a penalization linear with the number of spatial predictors introduced is computed as `(1 / total spatial predictors) * introduced spatial predictors` are recorded, and rescaled between 0 and 1. The optimization criteria is computed as `max(1 - Moran's I, p-value binary) + (weight.r.squared * R-squared) - (weight.penalization.n.predictors * penalization)`. The predictors from the first one to the one with the highest optimization criteria then selected as the best ones in reducing the spatial correlation of the model residuals, and used along with `data` to fit the final spatial model that is then returned by the function.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  data("distance_matrix")
#'  data("plant_richness_df")
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
#'  rf.repeat <- rf_repat(
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
#'  }
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
  trees.per.variable = NULL,
  scaled.importance = TRUE,
  repetitions = 1,
  keep.models = FALSE,
  method = c(
    "mem.moran.sequential", #mem ordered by their Moran's I.
    "mem.effect.sequential", #mem added in order of effect.
    "mem.effect.optimized", #mem added maximizing their joint effect.
    "hengl", #all distance matrix columns as predictors
    "hengl.moran.sequential", #distance matrix columns added in the order of their Moran's I
    "hengl.effect.sequential", #distance matrix columns added in order of their effect.
    "hengl.effect.optimized", #distance matrix columns added maximizing their joint effect.
    "pca.moran.sequential", #pca factors added in order of their Moran's I.
    "pca.effect.sequential", #pca factors added in order of effect
    "pca.effect.optimized" #pca factors added maximizing their joint effect.
  ),
  max.spatial.predictors = 1000,
  weight.r.squared = NULL,
  weight.penalization.n.predictors = NULL,
  seed = NULL,
  verbose = TRUE,
  n.cores = NULL,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = NULL,
  cluster.port = 11000
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
      "mem.effect.optimized",
      "hengl",
      "hengl.moran.sequential",
      "hengl.effect.sequential",
      "hengl.effect.optimized",
      "pca.moran.sequential",
      "pca.effect.sequential",
      "pca.effect.optimized"
    ),
    several.ok = FALSE
  )

  #getting arguments from model
  if(!is.null(model)){

    ranger.arguments <- NULL
    data <- NULL
    dependent.variable.name <- NULL
    predictor.variable.names <- NULL
    distance.matrix = NULL
    distance.thresholds <- NULL
    trees.per.variable <- NULL
    scaled.importance <- TRUE
    ranger.arguments <- model$ranger.arguments
    list2env(ranger.arguments, envir=environment())
    seed <- NULL
    importance <- "permutation"

  }

  #FITTING NON-SPATIAL MODEL
  #######################################################
  if(is.null(model)){

    if(repetitions == 1){

      model <- rf(
        data = data,
        dependent.variable.name = dependent.variable.name,
        predictor.variable.names = predictor.variable.names,
        distance.matrix = distance.matrix,
        distance.thresholds = distance.thresholds,
        ranger.arguments = ranger.arguments,
        seed = seed,
        verbose = FALSE
      )

    } else {

      model <- rf_repeat(
        data = data,
        dependent.variable.name = dependent.variable.name,
        predictor.variable.names = predictor.variable.names,
        distance.matrix = distance.matrix,
        distance.thresholds = distance.thresholds,
        ranger.arguments = ranger.arguments,
        trees.per.variable = trees.per.variable,
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

    }

  }

  #extracting autocorrelation of the residuals
  interpretation <- NULL
  model.moran.i <- model$spatial.correlation.residuals$per.distance %>%
    dplyr::arrange(dplyr::desc(moran.i)) %>%
    dplyr::filter(interpretation == "Positive spatial correlation")

  #if residuals are not autocorrelated, return original model
  if(nrow(model.moran.i) == 0){

    if(verbose == TRUE){
      message("Residuals are not spatially correlated, this model is good to go!")
      suppressWarnings(print(model$spatial.correlation.residuals$plot))
    }

    return(model)

  }#END OF NON SPATIAL MODEL


  #GENERATING SPATIAL PREDICTORS
  #########################################################

  #HENGL
  if(method %in% c(
    "hengl",
    "hengl.moran.sequential",
    "hengl.effect.sequential",
    "hengl.effect.optimized"
  )
  ){

    #change name of distance matrix
    spatial.predictors.df <- distance.matrix
    colnames(spatial.predictors.df) <- rownames(spatial.predictors.df) <- paste0("spatial_predictor_", 1:ncol(distance.matrix))

    if(verbose == TRUE){
      message("Using the distance matrix columns as spatial predictors.")
    }


  }

  #PCA
  if(method %in% c(
    "pca.moran.sequential",
    "pca.effect.sequential",
    "pca.effect.optimized"
  )
  ){

    #computing pca factors for pca methods
    spatial.predictors.df <- pca_multithreshold(
      x = distance.matrix,
      distance.thresholds =  model.moran.i$distance.threshold,
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
    "mem.effect.optimized"
  )
  ){


    #computing mem
    spatial.predictors.df <- mem_multithreshold(
      x = distance.matrix,
      distance.thresholds =  model.moran.i$distance.threshold,
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
    "hengl.effect.optimized",
    "pca.effect.sequential",
    "pca.effect.optimized",
    "mem.effect.sequential",
    "mem.effect.optimized"
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
      reference.moran.i = model$spatial.correlation.residuals$max.moran,
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
      message("Selecting spatial predictors in the order of the ranking (sequentially)")
    }

    #setting default weights
    if(is.null(weight.r.squared)){weight.r.squared <- 0.75}
    if(is.null(weight.penalization.n.predictors)){weight.penalization.n.predictors <- 0.25}

    #setting specific weights
    if(method %in% c("pca.moran.sequential", "pca.effect.sequential")){
      weight.penalization.n.predictors <- 0.1
      weight.r.squared <- 0.5
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
      cluster.ips = cluster.ips,
      cluster.cores = cluster.cores,
      cluster.user = cluster.user,
      cluster.port = cluster.port
    )

    #broadcast spatial.predictors.selected
    spatial.predictors.selected <- spatial.predictors.selection$best.spatial.predictors

  }


  #OPTIMIZED SELECTION OF SPATIAL PREDICTORS
  if(method %in% c(
    "hengl.effect.optimized",
    "pca.effect.optimized",
    "mem.effect.optimized"
  )
  ){

    if(verbose == TRUE){
      message("Selecting spatial predictors by optimizing their joint effect on the Moran's I of the model's residuals")
    }

    #setting weights
    if(is.null(weight.r.squared)){weight.r.squared <- 0.25}
    if(is.null(weight.penalization.n.predictors)){weight.penalization.n.predictors <- 0}

    #selecting spatial predictors by maximizing their joint effect
    spatial.predictors.selection <- select_spatial_predictors_optimized(
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
      cluster.cores = cluster.ips,
      cluster.user = cluster.ips,
      cluster.port = cluster.ips
    )

    #broadcast spatial.predictors.selected
    spatial.predictors.selected <- spatial.predictors.selection$best.spatial.predictors

  }


  #FITTING SPATIAL MODEL
  ######################

  #subsetting spatial predictors
  spatial.predictors.df <- spatial.predictors.df[, spatial.predictors.selected]

  #prepare data with best pca factors
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

  #fitting a single spatial model
  #find repetitions in model
  if(inherits(model, "rf_repeat")){
    repetitions <- model$ranger.arguments$repetitions
  }

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

  } else {

    model.spatial <- rf_repeat(
      data = data.spatial,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names.spatial,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments,
      trees.per.variable = trees.per.variable,
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

    }

  #PREPARING PLOTTING DATAFRAMES
  #################

  #MORAN'S I AFTER AND BEFORE
  after.df <- model.spatial$spatial.correlation.residuals$per.distance
  after.df$model <- "Spatial"
  before.df <- model$spatial.correlation.residuals$per.distance
  before.df$model <- "Non-spatial"
  moran.i.plot.df <- rbind(before.df, after.df)
  model.spatial$spatial.correlation.residuals$per.distance <- moran.i.plot.df

  if(inherits(model.spatial, "rf")){
    print_moran(
      x = moran.i.plot.df,
      caption = paste0("Moran's I of the spatial and non-spatial models."),
      verbose = verbose
    )
  }

  if(inherits(model.spatial, "rf_repeat")){

    after.df <- model.spatial$spatial.correlation.residuals$per.repetition
    after.df$model <- "Spatial"
    before.df <- model$spatial.correlation.residuals$per.repetition
    before.df$model <- "Non-spatial"
    moran.i.plot.df <- rbind(before.df, after.df)
    model.spatial$spatial.correlation.residuals$per.repetition <- moran.i.plot.df

    print_moran(
      x = moran.i.plot.df,
      caption = paste0(
        "Moran's I of the spatial and non-spatial models (averaged of ",
        repetitions,
        " repetitions)."),
      verbose = verbose
    )
  }

  #moran's I plot
  model.spatial$spatial.correlation.residuals$plot <- plot_moran(
    x = moran.i.plot.df,
    verbose = verbose
  )


  #VARIABLE IMPORTANCE
  #complete df
  if(inherits(model.spatial, "rf")){
    importance.df <- model.spatial$variable.importance$per.variable
  }
  if(inherits(model.spatial, "rf_repeat")){
    importance.df <- model.spatial$variable.importance$per.repetition
  }

  #spatial predictors only
  spatial.predictors.plot.df <- importance.df[grepl(
    "spatial_predictor",
    importance.df$variable
  ),]
  spatial.predictors.plot.df$variable <- "spatial_predictors"

  #non-spatial rpedictors
  non.spatial.predictors.plot.df <- importance.df[!grepl(
    "spatial_predictor",
    importance.df$variable
  ),]

  #joining for plot
  importance.plot.df <- rbind(
    spatial.predictors.plot.df,
    non.spatial.predictors.plot.df
  )


  #AGGREGATED DF OF spatial_predictor IMPORTANCE
  #min, max, median and mean of the spatial predictors
  spatial.predictors.stats <- data.frame(
    variable = c(
      "spatial_predictors (max)",
      "spatial_predictors (min)",
      "spatial_predictors (mean)",
      "spatial_predictors (median)"
    ),
    importance = c(
      max(spatial.predictors.plot.df$importance),
      min(spatial.predictors.plot.df$importance),
      mean(spatial.predictors.plot.df$importance),
      median(spatial.predictors.plot.df$importance)
    )
  )

  if(inherits(model.spatial, "rf")){
    importance.df <- rbind(
      non.spatial.predictors.plot.df,
      spatial.predictors.stats
      ) %>%
      dplyr::arrange(dplyr::desc(importance))
  }

  if(inherits(model.spatial, "rf_repeat")){
    importance.df <- non.spatial.predictors.plot.df %>%
      dplyr::group_by(variable) %>%
      dplyr::summarise(importance = mean(importance)) %>%
      as.data.frame() %>%
      rbind(spatial.predictors.stats) %>%
      dplyr::arrange(dplyr::desc(importance))
  }

  #adding it to the variable importance slot
  model.spatial$variable.importance$spatial.predictors <- importance.plot.df
  model.spatial$variable.importance$spatial.predictor.stats <- importance.df
  model.spatial$variable.importance$plot <- plot_importance(
    x = importance.plot.df,
    verbose = verbose
  )

  #COMPARING SPATIAL AND NON-SPATIAL MODELS
  comparison.df <- data.frame(
    model = c("Non-spatial", "Spatial"),
    r.squared = c(
      mean(model$performance$r.squared),
      mean(model.spatial$performance$r.squared)
      ),
    rmse = c(
      mean(model$performance$rmse),
      mean(model.spatial$performance$rmse)
      ),
    nrmse = c(
      mean(model$performance$nrmse),
      mean(model.spatial$performance$nrmse)
      ),
    max.moran.i = c(
      model$spatial.correlation.residuals$max.moran,
      model.spatial$spatial.correlation.residuals$max.moran
    )
  )

  if(verbose == TRUE){
    cat(" \n")
    colnames(comparison.df) <- c("Model", "R squared", "RMSE", "NRMSE", "Max. Moran's I")
    comparison.hux <-
      huxtable::hux(comparison.df) %>%
      huxtable::set_bold(row = 1, col = huxtable::everywhere, value = TRUE) %>%
      huxtable::set_bold(col = 1, row = huxtable::everywhere, value = TRUE) %>%
      huxtable::set_all_borders(TRUE)
    huxtable::number_format(comparison.hux)[2:3, 2:5] <- 3
    if(repetitions == 1){
      huxtable::caption(comparison.hux) <- "Comparing spatial and non-spatial models"
    } else {
      huxtable::caption(comparison.hux) <- paste0(
        "Comparing spatial and non-spatial models (average of ",
        repetitions,
        " repetitions."
        )
    }
    huxtable::print_screen(comparison.hux, colnames = FALSE)
  }

  #adding data to the model
  model.spatial$performance.comparison <- comparison.df

  #adding spatial method and predictors to the model
  model.spatial$selection.spatial.predictors <- list()
  model.spatial$selection.spatial.predictors$method <- method
  model.spatial$selection.spatial.predictors$names <- spatial.predictors.selected
  if(exists("spatial.predictors.selection")){
    model.spatial$selection.spatial.predictors$optimization <- spatial.predictors.selection$optimization
    model.spatial$selection.spatial.predictors$plot <- plot_optimization(
      x = spatial.predictors.selection$optimization,
      verbose = verbose
    )
  }

  #adding class
  if(inherits(model.spatial, "rf_repeat")){
    class(model.spatial) <- c("ranger", "rf_spatial", "rf_repeat")
  } else {
    class(model.spatial) <- c("ranger", "rf_spatial", "rf")
  }


  #return output
  model.spatial

}
