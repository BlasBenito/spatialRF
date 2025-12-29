#' @title Fits spatial random forest models
#' @description Fits spatial random forest models using different methods to generate, rank, and select spatial predictors acting as proxies of spatial processes not considered by the non-spatial predictors. The end goal is providing the model with information about the spatial structure of the data to minimize the spatial correlation (Moran's I) of the model residuals and generate honest variable importance scores.
#' @param model A model fitted with [rf()]. If used, the arguments `data`, `dependent.variable.name`, `predictor.variable.names`, `distance.matrix`, `distance.thresholds`, `ranger.arguments`, and `scaled.importance` are taken directly from the model definition. Default: NULL
#' @param data Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param dependent.variable.name Character string with the name of the response variable. Must be in the column names of `data`. If the dependent variable is binary with values 1 and 0, the argument `case.weights` of `ranger` is populated by the function [case_weights()]. Default: `NULL`
#' @param predictor.variable.names Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param distance.matrix Squared matrix with the distances among the records in `data`. The number of rows of `distance.matrix` and `data` must be the same. If not provided, the computation of the Moran's I of the residuals is omitted. Default: `NULL`
#' @param distance.thresholds Numeric vector with distances in the same units as `distance.matrix` Distances below each distance threshold are set to 0 on separated copies of the distance matrix to compute Moran's I at different neighborhood distances. If `NULL`, it defaults to `seq(0, max(distance.matrix)/2, length.out = 4)` (defined by [default_distance_thresholds()]). Default: `NULL`
#' @param xy (optional) Data frame or matrix with two columns containing coordinates and named "x" and "y". It is not used by this function, but it is stored in the slot `ranger.arguments$xy` of the model, so it can be used by [rf_evaluate()] and [rf_tuning()]. Default: `NULL`
#' @param ranger.arguments Named list with \link[ranger]{ranger} arguments (other arguments of this function can also go here). All \link[ranger]{ranger} arguments are set to their default values except for 'importance', that is set to 'permutation' rather than 'none'. Please, consult the help file of \link[ranger]{ranger} if you are not familiar with the arguments of this function.
#' @param scaled.importance Logical. If `TRUE`, and 'importance = "permutation', the function scales 'data' with \link[base]{scale} and fits a new model to compute scaled variable importance scores. Default: `TRUE`
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
#' @param seed Integer, random seed to facilitate reproducibility. Default: `1`.
#' @param verbose Logical. If TRUE, messages and plots generated during the execution of the function are displayed, Default: `TRUE`
#' @param n.cores Integer. Number of threads for ranger's internal parallelization. Default: `NULL` (auto-detected: if user has set a parallel plan via `future::plan()`, defaults to 1; otherwise defaults to `future::availableCores(omit = 1)`). Set to 1 for debugging. Note: When using a parallel plan (multiple workers), setting n.cores > 1 may cause oversubscription. The function will warn if this occurs.
#' @return A ranger model with several new slots:
#' \itemize{
#'   \item `ranger.arguments`: Values of the arguments used to fit the ranger model.
#'   \item `importance`: A list containing the vector of variable importance as originally returned by ranger (scaled or not depending on the value of 'scaled.importance'), a data frame with the predictors ordered by their importance, and a ggplot showing the importance values.
#'   \item `performance`: With the out-of-bag R squared, pseudo R squared, RMSE and NRMSE of the model.
#'   \item `residuals`: residuals, normality test of the residuals computed with [residuals_test()], and spatial autocorrelation of the residuals computed with [moran_multithreshold()].
#'   \item `spatial`: A list with four slots:
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
#'   \item `"hengl"`: named after the method RFsp presented in the paper "Random forest as a generic framework for predictive modeling of spatial and spatio-temporal variables", by Hengl et al. (2018), where the authors propose to use the distance matrix among records as predictors in spatial random forest models (RFsp method). In this function, all methods starting with "hengl" use either the complete distance matrix, or select columns of the distance matrix as spatial predictors.
#'   \item `"mem"`: Generates Moran's Eigenvector Maps, that is, the eigenvectors of the double-centered weights of the distance matrix. The method is described in "Spatial modelling: a comprehensive framework for principal coordinate analysis of neighbour matrices (PCNM)", by Dray et al. (2006), and "Statistical methods for temporal and space–time analysis of community composition data", by Legendre and Gauthier (2014).
#'   \item `"pca"`: Computes spatial predictors from the principal component analysis of a weighted distance matrix (see [weights_from_distance_matrix()]). This is an experimental method, use with caution.
#' }
#' Methods to rank spatial predictors (see [rank_spatial_predictors()]):
#' \itemize{
#'   \item `"moran"`: Computes the Moran's I of each spatial predictor, selects the ones with positive values, and ranks them from higher to lower Moran's I.
#'   \item `"effect"`: If a given non-spatial random forest model is defined as `y = p1 + ... + pn`, being `p1 + ... + pn` the set of predictors, for every spatial predictor generated (`spX`) a spatial model `y = p1 + ... + pn + spX` is fitted, and the Moran's I of its residuals is computed. The spatial predictors are then ranked by how much they help to reduce spatial autocorrelation between the non-spatial and the spatial model.
#' }
#' Methods to select spatial predictors:
#' \itemize{
#'   \item `"sequential"` (see [select_spatial_predictors_sequential()]): The spatial predictors are added one by one in the order they were ranked, and once all spatial predictors are introduced, the best first n predictors are selected. This method is similar to the one employed in the MEM methodology (Moran's Eigenvector Maps) described in the paper "Spatial modelling: a comprehensive framework for principal coordinate analysis of neighbour matrices (PCNM)", by Dray et al. (2006), and "Statistical methods for temporal and space–time analysis of community composition data", by Legendre and Gauthier (2014). This method generally introduces tens of predictors into the model, but usually offers good results.
#'   \item `"recursive"` (see [select_spatial_predictors_recursive()]): This method tries to find the smallest combination of spatial predictors that reduce the spatial correlation of the model's residuals the most. The algorithm goes as follows: 1. The first ranked spatial predictor is introduced into the model; 2. the remaining predictors are ranked again using the "effect" method, using the model in 1. as reference. The first spatial predictor in the resulting ranking is then introduced into the model, and the steps 1. and 2. are repeated until spatial predictors stop having an effect in reducing the Moran's I of the model residuals. This method takes longer to compute, but generates smaller sets of spatial predictors. This is an experimental method, use with caution.
#' }
#' Once ranking procedure is completed, an algorithm is used to select the minimal subset of spatial predictors that reduce the most the Moran's I of the residuals: for each new spatial predictor introduced in the model, the Moran's I of the residuals, it's p-value, a binary version of the p-value (0 if < 0.05 and 1 if >= 0.05), the R-squared of the model, and a penalization linear with the number of spatial predictors introduced (computed as `(1 / total spatial predictors) * introduced spatial predictors`) are rescaled between 0 and 1. Then, the optimization criteria is computed as `max(1 - Moran's I, p-value binary) + (weight.r.squared * R-squared) - (weight.penalization.n.predictors * penalization)`. The predictors from the first one to the one with the highest optimization criteria are then selected as the best ones in reducing the spatial correlation of the model residuals, and used along with `data` to fit the final spatial model.
#' @examples
#'
#' if (interactive()) {
#'   data(
#'     plants_df,
#'     plants_response,
#'     plants_predictors,
#'     plants_distance,
#'     plants_rf
#'   )
#'
#'   #subset to speed up example
#'   idx <- 1:100
#'   plants_df <- plants_df[idx, ]
#'   plants_distance <- plants_distance[idx, idx]
#'
#'   #fit spatial model from scratch
#'   m_spatial <- rf_spatial(
#'     data = plants_df,
#'     dependent.variable.name = plants_response,
#'     predictor.variable.names = plants_predictors,
#'     distance.matrix = plants_distance,
#'     distance.thresholds = c(100, 1000, 2000),
#'     method = "mem.moran.sequential",
#'     ranger.arguments = list(num.trees = 30),
#'     n.cores = 1
#'   )
#'
#'   plot_residuals_diagnostics(m_spatial)
#'
#'   #optimization of MEM selection
#'   plot_optimization(m_spatial)
#'
#'   #from non-spatial to spatial model
#'   m_spatial <- rf_spatial(
#'     model = plants_rf
#'     )
#'
#' }
#'
#'
#' @rdname rf_spatial
#' @family main_models
#' @autoglobal
#' @export
rf_spatial <- function(
  model = NULL,
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  xy = NULL,
  ranger.arguments = NULL,
  scaled.importance = TRUE,
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
  seed = 1,
  verbose = TRUE,
  n.cores = NULL
) {
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

  # FITTING NON-SPATIAL model or
  # GETTING ARGUMENTS FROM model
  #####################################

  #if model is not provided
  if (is.null(model)) {
    #stopping if no data
    if (is.null(data)) {
      stop("The argument 'data' is missing.")
    } else {
      #coerce to data frame if tibble
      if (inherits(data, "tbl_df") || inherits(data, "tbl")) {
        data <- as.data.frame(data)
      }
    }

    #coerce to data frame if tibble
    if (inherits(xy, "tbl_df") || inherits(xy, "tbl")) {
      xy <- as.data.frame(xy)
    }

    #stopping if no distance matrix
    if (is.null(distance.matrix)) {
      stop("The argument 'distance.matrix' is missing.")
    }
    #stopping if no dependent.variable.name
    if (is.null(dependent.variable.name)) {
      stop("The argument 'dependent.variable.name' is missing.")
    }
    #stopping if no predictor.variable.names
    if (is.null(predictor.variable.names)) {
      stop("The argument 'predictor.variable.names' is missing.")
    } else {
      #predictor.variable.names comes from auto_vif or auto_cor
      if (inherits(predictor.variable.names, "variable_selection")) {
        predictor.variable.names <- predictor.variable.names$selected.variables
      }
    }

    #fitting non-spatial model
    model <- rf(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      xy = xy,
      ranger.arguments = ranger.arguments,
      scaled.importance = FALSE,
      seed = seed,
      n.cores = n.cores,
      verbose = FALSE
    )
  } else {
    #getting arguments from model
    ranger.arguments <- model$ranger.arguments
    data <- ranger.arguments$data
    dependent.variable.name <- ranger.arguments$dependent.variable.name
    predictor.variable.names <- ranger.arguments$predictor.variable.names
    distance.matrix <- ranger.arguments$distance.matrix
    if (is.null(distance.matrix)) {
      stop("The argument 'distance.matrix' is missing.")
    }
    distance.thresholds <- ranger.arguments$distance.thresholds
    if (is.null(distance.thresholds)) {
      distance.thresholds <- default_distance_thresholds(
        distance.matrix = distance.matrix
      )
    }
    scaled.importance <- ranger.arguments$scaled.importance
    seed <- model$ranger.arguments$seed
  }

  #reference moran's I for selection of spatial predictors
  if (!is.null(model$residuals$autocorrelation$max.moran)) {
    reference.moran.i <- model$residuals$autocorrelation$max.moran
  } else {
    reference.moran.i <- 1
  }

  #extracting autocorrelation of the residuals
  model.moran.i <- model$residuals$autocorrelation$per.distance
  model.moran.i <- model.moran.i[
    order(model.moran.i$moran.i, decreasing = TRUE),
  ]
  model.moran.i <- model.moran.i[
    model.moran.i$interpretation == "Positive spatial correlation",
  ]

  #if residuals are not autocorrelated, return original model
  if (nrow(model.moran.i) == 0) {
    if (verbose) {
      message(
        "The model residuals are not spatially correlated, there is no need to fit a spatial model"
      )
    }
  } else {
    #if residuals are autocorrelated

    if (verbose) {
      message(
        "The model residuals are spatially correlated, fitting a spatial model."
      )
    }
  }

  #getting distance thresholds with positive AC
  #used to generate spatial predictors for these thresholds alone
  distance.thresholds.with.ac <- model.moran.i$distance.threshold

  #GENERATING SPATIAL PREDICTORS
  #########################################################

  #HENGL
  if (
    method %in%
      c(
        "hengl",
        "hengl.moran.sequential",
        "hengl.effect.sequential",
        "hengl.effect.recursive"
      )
  ) {
    #change name of distance matrix
    spatial.predictors.df <- distance.matrix
    colnames(spatial.predictors.df) <- rownames(
      spatial.predictors.df
    ) <- paste0(
      "spatial_predictor_",
      seq(
        1,
        ncol(distance.matrix)
      )
    )
    spatial.predictors.selected <- colnames(spatial.predictors.df)

    if (verbose) {
      message("Using the distance matrix columns as spatial predictors.")
    }
  }

  #PCA
  if (
    method %in%
      c(
        "pca.moran.sequential",
        "pca.effect.sequential",
        "pca.effect.recursive"
      )
  ) {
    #computing pca factors for pca methods
    spatial.predictors.df <- pca_multithreshold(
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds.with.ac,
      max.spatial.predictors = max.spatial.predictors
    )

    if (verbose) {
      message("Using PCA factors of the distance matrix as spatial predictors.")
    }
  }

  #MEM
  if (
    method %in%
      c(
        "mem.moran.sequential",
        "mem.effect.sequential",
        "mem.effect.recursive"
      )
  ) {
    #computing mem
    spatial.predictors.df <- mem_multithreshold(
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds.with.ac,
      max.spatial.predictors = max.spatial.predictors
    )

    if (verbose) {
      message("Using Moran's Eigenvector Maps as spatial predictors.")
    }
  }

  #RANKING SPATIAL PREDICTORS
  ###########################################################

  #SELECTING RANKING METHOD
  if (method %in% "hengl") {
    ranking.method <- NULL
  }

  if (
    method %in%
      c(
        "hengl.moran.sequential",
        "pca.moran.sequential",
        "mem.moran.sequential"
      )
  ) {
    ranking.method <- "moran"
    if (verbose) {
      message("Ranking spatial predictors by their Moran's I.")
    }
  }

  if (
    method %in%
      c(
        "hengl.effect.sequential",
        "hengl.effect.recursive",
        "pca.effect.sequential",
        "pca.effect.recursive",
        "mem.effect.sequential",
        "mem.effect.recursive"
      )
  ) {
    ranking.method <- "effect"
    if (verbose) {
      message(
        "Ranking spatial predictors by how much they reduce Moran's I of the model residuals."
      )
    }
  }

  #ranking
  if (!is.null(ranking.method)) {
    #removes redundant spatial predictors
    spatial.predictors.df <- filter_spatial_predictors(
      data = data,
      predictor.variable.names = predictor.variable.names,
      spatial.predictors.df = spatial.predictors.df,
      cor.threshold = 0.50
    )

    #check if any spatial predictors remain after filtering
    if (ncol(spatial.predictors.df) == 0) {
      if (verbose) {
        message(
          "No spatial predictors remain after filtering. Returning non-spatial model."
        )
      }
      return(model)
    }

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
      reference.moran.i = reference.moran.i,
      verbose = FALSE,
      n.cores = n.cores
    )
  }

  #SELECTING SPATIAL PREDICTORS (if method is not "hengl")
  ######################################################

  #SEQUENTIAL SELECTION OF SPATIAL PREDICTORS
  if (
    method %in%
      c(
        "hengl.moran.sequential",
        "mem.moran.sequential",
        "pca.moran.sequential",
        "hengl.effect.sequential",
        "mem.effect.sequential",
        "pca.effect.sequential"
      )
  ) {
    if (verbose) {
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
      n.cores = n.cores
    )

    #names of the selected spatial predictors
    spatial.predictors.selected <- as.character(
      spatial.predictors.selection$best.spatial.predictors
    )
  }

  #recursive SELECTION OF SPATIAL PREDICTORS
  if (
    method %in%
      c(
        "hengl.effect.recursive",
        "pca.effect.recursive",
        "mem.effect.recursive"
      )
  ) {
    if (verbose) {
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
      n.cores = n.cores
    )

    #broadcast spatial.predictors.selected
    spatial.predictors.selected <- spatial.predictors.selection$best.spatial.predictors
  }

  #FITTING SPATIAL MODEL
  ######################

  #subsetting spatial predictors
  if (method != "hengl") {
    spatial.predictors.df <- spatial.predictors.df[,
      spatial.predictors.selected,
      drop = FALSE
    ]
  }

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

  #fitting model
  model.spatial <- rf(
    data = data.spatial,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names.spatial,
    distance.matrix = distance.matrix,
    distance.thresholds = distance.thresholds,
    xy = xy,
    ranger.arguments = ranger.arguments,
    seed = seed,
    n.cores = n.cores,
    verbose = FALSE
  )

  class(model.spatial) <- c("rf", "rf_spatial", "ranger")

  #add moran's I plot
  model.spatial$residuals$autocorrelation$plot <- plot_moran(
    model.spatial,
    verbose = FALSE
  )

  #preparing global variable importance
  model.spatial$importance <- prepare_importance_spatial(model = model.spatial)

  #preparing local variable importance
  model.spatial$importance$local <- model.spatial$variable.importance.local

  #adding spatial method and predictors to the model
  model.spatial$spatial <- list()
  model.spatial$spatial$method <- method
  model.spatial$spatial$names <- spatial.predictors.selected
  model.spatial$spatial$spatial.predictors <- spatial.predictors.df

  if (exists("spatial.predictors.selection")) {
    model.spatial$spatial$optimization <- spatial.predictors.selection$optimization
    model.spatial$spatial$plot <- plot_optimization(
      spatial.predictors.selection$optimization,
      verbose = FALSE
    )
  }

  if (verbose) {
    message("Details about the spatial predictors stored in model$spatial.")
  }

  if (verbose) {
    print(model.spatial)
    plot_importance(model.spatial)
    plot_residuals_diagnostics(model.spatial)
  }

  #return output
  model.spatial
}
