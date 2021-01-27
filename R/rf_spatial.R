#' @title rf_spatial
#' @description Fits spatial random forest models using different methods to generate, rank, and select spatial predictors. The end goal is to provide the model with information about the spatial structure of the data in order to minimize the spatial correlation of the residuals. See Details for a description of the methods.
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
#' @param verbose Boolean. If TRUE, messages and plots generated during the execution of the function are displayed, Default: TRUE
#' @param scaled.importance (optional) boolean. If TRUE, and 'importance = "permutation', the function scales 'data' with [scale_robust] and fits a new model to compute scaled variable importance scores. Default: TRUE
#' @param seed (optional) integer, random seed to facilitate reproducibility. If set to a given number, the returned model is always the same. Only relevant if `repetitions = 1`. Default: NULL
#' @param repetitions integer, number of random forest models to fit. Default: 5
#' @param keep.models boolean, if TRUE, the fitted models are returned in the "models" slot. Default: FALSE.
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
#'   \item{variable.importance}{a list containing a data frame with the variable importance obtained on each iteration (df.wide), the mean importance of each predictor across repetitions (df), a long version of the df.wide data frame to facilitate plotting (df.long), and a boxplot showing the distribution of the importance scores across repetitions}
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
#'  data = data,
#'  dependent.variable.name = dependent.variable.name,
#'  predictor.variable.names = predictor.variable.names,
#'  distance.matrix = distance.matrix,
#'  distance.thresholds = distance.thresholds,
#'  method = "mem.moran.sequential",
#'  seed = 10
#'  )
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{arrange}},\code{\link[dplyr]{desc}},\code{\link[dplyr]{filter}}
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{geom_point}},\code{\link[ggplot2]{scale_colour_viridis_d}},\code{\link[ggplot2]{geom_path}},\code{\link[ggplot2]{labs}},\code{\link[ggplot2]{geom_boxplot}},\code{\link[ggplot2]{geom_abline}},\code{\link[ggplot2]{scale_manual}},\code{\link[ggplot2]{theme}}
#' @rdname rf_spatial
#' @export
#' @importFrom ggplot2 ggplot aes geom_point scale_color_viridis_c geom_path labs xlab ylab ggtitle geom_boxplot geom_hline geom_line scale_colour_manual theme
rf_spatial <- function(
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
    "hengl", #all distance matrix columns as predictors
    "hengl.moran.sequential", #distance matrix columns added in the order of their Moran's I
    "hengl.effect.sequential", #distance matrix columns added in order of their effect.
    "hengl.effect.optimized", #distance matrix columns added maximizing their joint effect.
    "pca.moran.sequential", #pca factors added in order of their Moran's I.
    "pca.effect.sequential", #pca factors added in order of effect
    "pca.effect.optimized", #pca factors added maximizing their joint effect.
    "mem.moran.sequential", #mem ordered by their Moran's I.
    "mem.effect.sequential", #mem added in order of effect.
    "mem.effect.optimized" #mem added maximizing their joint effect.
  ),
  max.spatial.predictors = 1000,
  weight.r.squared = NULL,
  weight.penalization.n.predictors = NULL,
  verbose = TRUE,
  seed = NULL,
  n.cores = NULL,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = NULL,
  cluster.port = 11000
){

  #testing method argument
  method <- match.arg(method)

  #initializing "importance"
  if(!is.null(ranger.arguments$importance)){
    importance <- ranger.arguments$importance
  } else {
    importance <- "none"
  }

  #FITTING NON-SPATIAL MODEL
  #######################################################
  if(repetitions == 1){

    m.non.spatial <- rf(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments,
      seed = seed
    )

  } else {

    m.non.spatial <- rf_repeat(
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
      n.cores = n.cores,
      cluster.ips = cluster.ips,
      cluster.cores = cluster.cores,
      cluster.user = cluster.user,
      cluster.port = cluster.port
    )

  }

  #extracting autocorrelation of the residuals
  interpretation <- NULL
  m.non.spatial.moran.i <- m.non.spatial$spatial.correlation.residuals$df %>%
    dplyr::arrange(dplyr::desc(moran.i)) %>%
    dplyr::filter(interpretation == "Positive spatial correlation")

  #if residuals are not autocorrelated, return original model
  if(nrow(m.non.spatial.moran.i) == 0){

    if(verbose == TRUE){
      message("Residuals are not spatially correlated, this model is good to go!")
      suppressWarnings(print(m.non.spatial$spatial.correlation.residuals$plot))
    }

    return(m.non.spatial)

  }# END OF FITTING NON-SPATIAL MODEL


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
      distance.thresholds =  m.non.spatial.moran.i$distance.threshold,
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
    #computing pca factors for pca methods
    spatial.predictors.df <- mem_multithreshold(
      x = distance.matrix,
      distance.thresholds =  m.non.spatial.moran.i$distance.threshold,
      max.spatial.predictors = max.spatial.predictors
    )

    if(verbose == TRUE){
      message("Using Moran's Eigenvector Maps of the double-centered distance weights as spatial predictors.")
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
      message("Ranking spatial predictors by how much they reduce the Moran's I of the model residuals.")
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
      reference.moran.i = m.non.spatial$spatial.correlation.residuals$max.moran,
      n.cores = n.cores,
      cluster.ips = cluster.ips,
      cluster.cores = cluster.cores,
      cluster.user = cluster.user,
      cluster.port = cluster.port
    )

  }

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

    #setting weights
    if(is.null(weight.r.squared)){weight.r.squared <- 0.75}
    if(is.null(weight.penalization.n.predictors)){weight.penalization.n.predictors <- 0.25}
    #setting specific weights
    if(method %in% c("pca.moran.sequential", "pca.effect.sequential")){
      weight.penalization.n.predictors <- 0.1
      weight.r.squared <- 0.5
    }

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

  #preparing plot of selection criteria
  if(exists("spatial.predictors.selection")){

    optimization <- NULL
    r.squared <- NULL
    spatial.predictor.index <- NULL
    plot.df <- spatial.predictors.selection$optimization
    selection.criteria.plot <- ggplot2::ggplot(data = plot.df) +
      ggplot2::aes(
        x = moran.i,
        y = r.squared,
        color = optimization,
        size = spatial.predictor.index
      ) +
      ggplot2::geom_point() +
      ggplot2::scale_color_viridis_c(direction = -1) +
      ggplot2::geom_point(
        data = plot.df[plot.df$selected, ],
        aes(
          x = moran.i,
          y = r.squared),
        colour="black",
        size = 5,
        shape = 1,
        alpha = 0.3
      ) +
      ggplot2::geom_path(data = plot.df[plot.df$selected, ],
                         aes(
                           x = moran.i,
                           y = r.squared
                         ),
                         size = 0.5,
                         color = "black",
                         alpha = 0.3
      ) +
      ggplot2::labs(
        size = "Added spatial predictors",
        color = "Weighted optimization index"
      ) +
      ggplot2::xlab("Moran's I of the model residuals") +
      ggplot2::ylab("Model's R-squared") +
      ggplot2::ggtitle("Selection of spatial predictors (selection path shown in gray)")

    if(verbose == TRUE){
      suppressWarnings(print(selection.criteria.plot))
    }

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
  if(repetitions == 1){

    #fitting model
    m.spatial <- rf(
      data = data.spatial,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names.spatial,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments,
      seed = seed
    )

    #preparing before and after plot.df
    after.df <- m.spatial$spatial.correlation.residuals$df
    after.df$model <- "Spatial"
    before.df <- m.non.spatial$spatial.correlation.residuals$df
    before.df$model <- "Non-spatial"
    plot.df <- rbind(before.df, after.df)
    m.spatial$spatial.correlation.residuals$df <- plot.df
    plot.df$p.value.binary <- "< 0.05"
    plot.df[plot.df$p.value >= 0.05, "p.value.binary"] <- ">= 0.05"

    #preparing variable importance plot
    if(!is.null(m.spatial$variable.importance)){
      importance.df <- m.spatial$variable.importance$df
      spatial.predictors.df <- importance.df[grepl(
        "spatial_predictor",
        importance.df$variable
      ),]
      spatial.predictors.df$variable <- "spatial_predictors"
      predictors.df <- importance.df[!grepl(
        "spatial_predictor",
        importance.df$variable
      ),]
      importance.df <- rbind(
        spatial.predictors.df,
        predictors.df
      )
      variable <- NULL
      importance.plot <- ggplot2::ggplot(data = importance.df) +
        ggplot2::aes(
          x = importance,
          y = reorder(
            variable,
            importance,
            FUN = max
          ),
          fill = importance
        ) +
        ggplot2::geom_point(size = 4, shape = 21) +
        ggplot2::scale_fill_viridis_c(direction = -1) +
        ggplot2::ylab("") +
        ggplot2::xlab("Variable importance")

      m.spatial$variable.importance$df <- importance.df
      m.spatial$variable.importance$plot <- importance.plot
    }

  } else {

    m.spatial <- rf_repeat(
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
      n.cores = n.cores,
      cluster.ips = cluster.ips,
      cluster.cores = cluster.cores,
      cluster.user = cluster.user,
      cluster.port = cluster.port
    )

    #preparing before and after plot.df
    after.df <- m.spatial$spatial.correlation.residuals$df.long
    after.df$model <- "Spatial"
    before.df <- m.non.spatial$spatial.correlation.residuals$df.long
    before.df$model <- "Non-spatial"
    plot.df <- rbind(before.df, after.df)
    m.spatial$spatial.correlation.residuals$df.long <- plot.df
    plot.df$p.value.binary <- "< 0.05"
    plot.df[plot.df$p.value >= 0.05, "p.value.binary"] <- ">= 0.05"

    #preparing importance plot
    if(!is.null(m.spatial$variable.importance)){
      importance.df <- m.spatial$variable.importance$df.long
      spatial.structure.df <- importance.df[grepl(
        "spatial_predictor",
        importance.df$variable
      ),]
      spatial.structure.df$variable <- "spatial_predictor"
      predictors.df <- importance.df[!grepl(
        "spatial_predictor",
        importance.df$variable
      ),]
      importance.df <- rbind(
        spatial.structure.df,
        predictors.df
      )

      #plot
      importance.plot <- ggplot2::ggplot(data = importance.df) +
        ggplot2::aes(
          x = importance,
          y = reorder(
            variable,
            importance,
            FUN = median
          ),
          fill = reorder(
            variable,
            importance,
            FUN = median
          )
        ) +
        ggplot2::geom_boxplot() +
        ggplot2::scale_fill_viridis_d(direction = -1, alpha = 0.8) +
        ggplot2::ylab("") +
        ggplot2::xlab("Variable importance") +
        ggplot2::theme(legend.position = "none")

      m.spatial$variable.importance$df <- importance.df
      m.spatial$variable.importance$plot <- importance.plot

    }

  }

  #plot Moran's I spatial vs non-spatial
  model <- NULL
  p.value.binary <- NULL
  m.spatial$spatial.correlation.residuals$plot <- ggplot2::ggplot(data = plot.df) +
    ggplot2::aes(
      x = distance.threshold,
      y = moran.i,
      color = model,
      size = p.value.binary
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      col = "gray10",
      size = 0.7,
      linetype = "dashed"
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_line(size = 1) +
    ggplot2::scale_colour_manual(values = c("#440154FF", "#35B779FF")) +
    ggplot2::xlab("Distance thresholds") +
    ggplot2::ylab("Moran's I of residuals") +
    ggplot2::ggtitle("Residuals' Moran's I of the spatial and non-spatial models") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(color = "Model", size = "Moran's I p-value")

  #printing Moran's I of residuals
  if(verbose == TRUE){
    suppressWarnings(print(m.spatial$spatial.correlation.residuals$plot))
  }

  #accuracy comparison
  comparison.df <- data.frame(
    r.squared = round(c(mean(m.non.spatial$r.squared), mean(m.spatial$r.squared)), 3),
    rmse = round(c(mean(m.non.spatial$rmse), mean(m.spatial$rmse)), 3),
    nrmse = round(c(mean(m.non.spatial$nrmse), mean(m.spatial$nrmse)), 3),
    moran.i.residuals = round(c(
      m.non.spatial$spatial.correlation.residuals$max.moran,
      m.spatial$spatial.correlation.residuals$max.moran
  ), 3)
  )
  rownames(comparison.df) <- c("Non-spatial model", "Spatial model")

  if(verbose == TRUE){
   message("Model comparison")
   print(comparison.df)
  }

  #adding data to the model
  m.spatial$performance.comparison <- comparison.df

  #adding spatial method and predictors to the model
  m.spatial$selection.spatial.predictors <- list()
  m.spatial$selection.spatial.predictors$method <- method
  m.spatial$selection.spatial.predictors$names <- spatial.predictors.selected
  if(exists("spatial.predictors.selection")){
    m.spatial$selection.spatial.predictors$df <- spatial.predictors.selection$optimization
    m.spatial$selection.spatial.predictors$plot <- selection.criteria.plot
  }

  #return output
  m.spatial

}
