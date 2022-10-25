#' @title Tuning of random forest hyperparameters via spatial cross-validation
#' @description Finds the optimal set of random forest hyperparameters `num.trees`, `mtry`, and `min.node.size` via grid search by maximizing the model's R squared, or AUC, if the response variable is binomial, via spatial cross-validation performed with [rf_evaluate()].
#' @param model A model fitted with [rf()]. If provided, the training data is taken directly from the model definition (stored in `model$ranger.arguments`). Default: `NULL`
#' @param num.trees Numeric integer vector with the number of trees to fit on each model repetition. Default: `c(100, 1000)`.
#' @param mtry Numeric integer vector, number of predictors to randomly select from the complete pool of predictors on each tree split. Default: `c(3, 6)`
#' @param min.node.size Numeric integer, minimal number of cases in a terminal node. Default: `c(5, 20)`
#' @param max.depth Numeric integer, maximal tree depth. `max.depth` and `min.node.size` cannot be used together in a model, so whenever the user provides values for both, separate models for the different values of `min.node.size` and `max.depth` will be compared. Default: `c(3, 6)`
#' @param xy Data frame or matrix with two columns containing coordinates and named "x" and "y". If `NULL`, the function will throw an error. Default: `NULL`
#' @param repetitions Integer, number of independent spatial folds to use during the cross-validation. Default: `30`.
#' @param training.fraction Proportion between 0.2 and 0.9 indicating the number of records to be used in model training. Default: `0.75`
#' @param seed Integer, random seed to facilitate reproduciblity. If set to a given number, the results of the function are always the same. Default: `1`.
#' @param verbose Logical. If TRUE, messages and plots generated during the execution of the function are displayed, Default: `TRUE`
#' @param n.cores Integer, number of cores used by #' @param n.cores Integer, number of cores used by \code{\link[ranger]{ranger}} for parallel execution (used as value for the argument `num.threads` in `ranger()`). Default: `NULL`
#' @param cluster A cluster definition generated with `parallel::makeCluster()` or \code{\link{start_cluster}}. Only advisable if you need to spread a large number of repetitions over the nodes of a large cluster when working with large data. If provided, overrides `n.cores`. The function does not stop a cluster, please remember to shut it down with `parallel::stopCluster(cl = cluster_name)` or `spatialRF::stop_cluster()` at the end of your pipeline. Default: `NULL`
#' @return A model with a new slot named `tuning`, with a data frame with the results of the tuning analysis.
#' @seealso [rf_evaluate()]
#' @examples
#' if(interactive()){
#'
#' #loading example data
#' data(
#'   ecoregions_df,
#'   ecoregions_distance_matrix,
#'   ecoregions_predictor_variable_names,
#'   ecoregions_dependent_variable_name
#'   )
#'
#' #fitting model to tune
#' out <- rf(
#'   data = ecoregions_df,
#'   dependent.variable.name = ecoregions_dependent_variable_name,
#'   predictor.variable.names = ecoregions_predictor_variable_names,
#'   distance.matrix = ecoregions_distance_matrix,
#'   distance.thresholds = 0,
#'   n.cores = 1,
#'   verbose = FALSE
#' )
#'
#' #model tuning
#' #please replace "n.cores = 1" with
#' #n.cores = parallel::detectCores() - 1"
#' #to improve performance
#' tuning <- rf_tuning(
#'   model = out,
#'   num.trees = c(100, 500),
#'   mtry = c(2, 8),
#'   min.node.size = c(5, 10),
#'   xy = ecoregions_df[, c("x", "y")],
#'   n.cores = 1
#' )
#'
#' #using the pipe and a cluster
#' #please replace "cluster.cores = 1" with
#' #"cluster.cores = parallel::detectCores() - 1"
#'
#' library(magrittr)
#' cluster <- start_cluster(cluster.cores = 1)
#'
#' out <- rf(
#'   data = ecoregions_df,
#'   dependent.variable.name = ecoregions_dependent_variable_name,
#'   predictor.variable.names = ecoregions_predictor_variable_names,
#'   distance.matrix = ecoregions_distance_matrix,
#'   xy = ecoregions_df[, c("x", "y")],
#'   distance.thresholds = 0,
#'   n.cores = 1
#' ) %>%
#' tuning <- rf_tuning(
#'   num.trees = c(100, 500),
#'   mtry = c(2, 8),
#'   min.node.size = c(5, 10)
#' )
#'
#' spatialRF::stop_cluster()
#'
#' }
#' @importFrom rlang sym
#' @importFrom foreach %do%
#' @rdname rf_tuning
#' @export
rf_tuning <- function(
  model = NULL,
  num.trees = c(100, 1000),
  mtry = c(3, 6),
  min.node.size = c(5, 20),
  max.depth = c(3, 6),
  xy = NULL,
  repetitions = 30,
  training.fraction = 0.75,
  seed = 1,
  verbose = TRUE,
  n.cores = parallel::detectCores() - 1,
  cluster = NULL
){

  #declaring variables
  metric <- NULL
  num.trees.i <- NULL
  mtry.i <- NULL
  min.node.size.i <- NULL
  max.depth.i <- NULL
  dependent.variable.name <- NULL
  distance.matrix <- NULL
  distance.thresholds <- NULL
  data <- NULL

  #HANDLING ARGUMENTS
  ######################################

  #saving tuning arguments from destruction
  num.trees.vector <- num.trees
  mtry.vector <- mtry
  min.node.size.vector <- min.node.size
  max.depth.vector <- max.depth

  #if model is provided
  if(!is.null(model)){

    #stopping if model is not of the right class
    if(!("rf" %in% class(model))){
      stop("The argument 'model' is not of the class 'rf'.")
    }

    #saving model class for later
    model.class <- class(model)

    #saving slots if it's an rf_spatial model
    if(inherits(model, "rf_spatial")){
      spatial <- model$spatial
    }

    #getting xy
    if(is.null(xy)){

      if(is.null(model$ranger.arguments$xy)){
        stop("The argument 'xy' is required for spatial cross-validation.")
      }

    } else {

      model$ranger.arguments$xy <- xy

      if(sum(c("x", "y") %in% colnames(xy)) < 2){
        stop("The column names of 'xy' must be 'x' and 'y'.")
      }

      if(nrow(xy) != nrow(data)){
        stop("nrow(xy) and nrow(data) (stored in model$ranger.arguments$data) must be the same.")
      }

    }

    #loading ranger.arguments onto the function's environment
    ranger.arguments <- model$ranger.arguments
    list2env(ranger.arguments, envir=environment())

    #predictor.variable.names comes from auto_vif or auto_cor
    if(inherits(predictor.variable.names, "variable_selection")){
      predictor.variable.names <- predictor.variable.names$selected.variables
    }

  } else {

    stop("The argument 'model' is empty, there is no model to tune!")

  }


  #END OF HANDLING ARGUMENTS
  ##########################

  #HANDLING PARALLELIZATION
  ##########################
  #here we don't use dopar in the loop, but in rf_evaluate
  `%iterator%` <- foreach::`%do%`

  if("cluster" %in% class(cluster)){

    #registering cluster
    doParallel::registerDoParallel(cl = cluster)

    #in-loop cores and ranger arguments
    in.loop.n.cores <- 1

  } else {

    #in-loop cores and ranger arguments
    in.loop.n.cores <- n.cores

    #NULL cluster
    cluster <- NULL

  }

  ##########################
  #END OF HANDLING PARALLELIZATION

  #testing if the data is binary
  if(.is_binary(
    x = dplyr::pull(data, dependent.variable.name)
  )
  ){
    metric <- "auc"
  } else {
    metric <- "r.squared"
  }


  #mtry
  mtry <- as.integer(mtry.vector)
    if(max(mtry) > length(predictor.variable.names)){
      if(verbose == TRUE){
        message("Maximum 'mtry' set to length(predictor.variable.names)")
      }
      mtry <- mtry[mtry < length(predictor.variable.names)]
    }


  #min.node.size
  min.node.size <- as.integer(min.node.size.vector)
    if(max(min.node.size) >= floor(nrow(data)/4)){
      min.node.size <- min.node.size[min.node.size <= floor(nrow(data)/4)]
      if(verbose == TRUE){
        message(paste0(
          "'min.node.size' values larger than ",
          floor(nrow(data)/4),
          " were removed."
        )
        )
      }
    }

  #num.trees
  num.trees <- as.integer(num.trees.vector)

  #max.depth
  max.depth <- as.integer(max.depth.vector)

  #combining values
  combinations.min.node.size <- expand.grid(
    num.trees = num.trees,
    mtry = mtry,
    min.node.size = min.node.size,
    max.depth = NA
  )

  combinations.max.depth <- expand.grid(
    num.trees = num.trees,
    mtry = mtry,
    min.node.size = NA,
    max.depth = max.depth
  )

  combinations <- rbind(
    combinations.min.node.size,
    combinations.max.depth
  )

  if(verbose == TRUE){
    message(
      paste0(
        "Exploring ",
        nrow(combinations),
        " combinations of hyperparameters."
      )
    )
  }

  #looping through combinations
  tuning <- foreach::foreach(
    num.trees.i = combinations$num.trees,
    mtry.i = combinations$mtry,
    min.node.size.i = combinations$min.node.size,
    max.depth.i = combinations$max.depth,
    .combine = "rbind",
    .verbose = FALSE
  ) %iterator% {

    #preparing ranger arguments
    if(!is.na(min.node.size.i)){
      ranger.arguments.i <- list(
        num.trees = num.trees.i,
        mtry = mtry.i,
        min.node.size = min.node.size.i,
        max.depth = NULL,
        importance = "none",
        num.threads = in.loop.n.cores,
        save.memory = TRUE,
        scaled.importance = FALSE
      )
    } else {
      ranger.arguments.i <- list(
        num.trees = num.trees.i,
        mtry = mtry.i,
        min.node.size = NULL,
        max.depth = max.depth.i,
        importance = "none",
        num.threads = in.loop.n.cores,
        save.memory = TRUE,
        scaled.importance = FALSE
      )
    }

    #fit and evaluate model with new hyperparameters
    m.i <- spatialRF::rf(
      model = model,
      ranger.arguments = ranger.arguments.i,
      seed = seed,
      n.cores = in.loop.n.cores,
      verbose = FALSE
    ) %>%
      spatialRF::rf_evaluate(
        repetitions = repetitions,
        training.fraction = training.fraction,
        metrics = metric,
        seed = seed,
        verbose = FALSE,
        n.cores = in.loop.n.cores,
        cluster = cluster
      )

    #getting performance measures
    m.i.performance <- spatialRF::get_evaluation_aggregated(m.i)
    m.i.performance <- m.i.performance[m.i.performance$model == "Testing", c("metric", "median")]

    #if the model is spatial
    if(inherits(model, "rf_spatial")){

      #getting interpretation of Moran's I if the model is rf_spatial
      moran.i.interpretation <- m.i$residuals$autocorrelation$per.distance$interpretation[1]

      #getting performance
      m.i.performance <- data.frame(
        r.squared = m.i.performance[m.i.performance$metric == metric, "median"],
        moran.i.interpretation = moran.i.interpretation
      )

    } else {

      #performance without Moran's I for non-spatial models
      m.i.performance <- data.frame(
        metric = m.i.performance[m.i.performance$metric == metric, "median"]
      )

    }
    colnames(m.i.performance)[1] <- metric

    return(m.i.performance)

  }#end of parallelized loop

  #binding with combinations
  tuning <- cbind(
    combinations,
    tuning
  ) %>%
    dplyr::arrange(dplyr::desc(!!rlang::sym(metric)))

  #getting metric name
  metric.name <- colnames(tuning)[!(colnames(tuning) %in% c("num.trees", "mtry", "min.node.size", "max.depth", "moran.i.interpretation"))]

  #preparing tuning list
  tuning.list <- list()
  tuning.list$metric <- metric.name
  tuning.list$tuning.df <- tuning


  #subset if rf_spatial
  if(inherits(model, "rf_spatial")){

    #remove results yielding
    tuning <- dplyr::filter(
      tuning,
      moran.i.interpretation == "No spatial correlation"
    )

    #stop if all results increase spatial autocorrelation of the residuals
    if(nrow(tuning) == 0){

      message("This spatial model cannot be tuned, all possible results increase spatial autocorrelation of the residuals")

      return(model)

    }

  }

  #best hyperparameters into ranger arguments
  ranger.arguments$num.trees <- tuning[1, "num.trees"]
  ranger.arguments$mtry <- tuning[1, "mtry"]
  ranger.arguments$min.node.size <- tuning[1, "min.node.size"]

  #fitting tuned model
  model.tuned <- rf(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    ranger.arguments = ranger.arguments,
    distance.matrix = distance.matrix,
    distance.thresholds = distance.thresholds,
    xy = xy,
    n.cores = n.cores,
    cluster = cluster,
    verbose = FALSE
  )

  #keeping class
  class(model.tuned) <- unique(
    c(
      class(model.tuned),
      model.class)
    )

  #adding tuning slot
  model.tuned$tuning <- tuning.list

  #adding the variable importance slot if rf_spatial
  if(inherits(model, "rf_spatial")){

    model.tuned$importance <- prepare_importance_spatial(model = model.tuned)

  }

  #comparing metric of the old model and the tuned one

  #evaluate input model
  model <- rf_evaluate(
    model = model,
    xy = xy,
    repetitions = repetitions,
    training.fraction = training.fraction,
    seed = seed,
    verbose = FALSE,
    n.cores = n.cores,
    cluster = cluster
  )

  #evaluate tuned model
  model.tuned <- rf_evaluate(
    model = model.tuned,
    xy = xy,
    repetitions = repetitions,
    training.fraction = training.fraction,
    seed = seed,
    verbose = FALSE,
    n.cores = n.cores,
    cluster = cluster
  )

  #extract r.squared values
  #tuned model
  new.performance <- round(
    model.tuned$evaluation$aggregated[model.tuned$evaluation$aggregated$model == "Testing" &
        model.tuned$evaluation$aggregated$metric == metric.name,
      "median"
    ],
    3
  )

  #old model
  old.performance <- round(
    model$evaluation$aggregated[
      model$evaluation$aggregated$model == "Testing" &
        model$evaluation$aggregated$metric == metric.name,
      "median"
    ],
    3
  )

  #performance difference
  performance.gain <- new.performance - old.performance

  #if there is r-squared gain
  if(performance.gain > 0){

    #plot tuning
    if(verbose == TRUE){
      suppressWarnings(plot_tuning(model.tuned))
      message("Best hyperparameters:")
      message(paste0("  - num.trees:     ", tuning[1, "num.trees"]))
      message(paste0("  - mtry:          ", tuning[1, "mtry"]))
      message(paste0("  - min.node.size: ", tuning[1, "min.node.size"]))
      message(paste0(
        "gain in ",
        metric.name,
        ": ",
        round(performance.gain, 4)
      )
      )
    }

    #adding gain
    model.tuned$tuning$performance.gain <- performance.gain

    #adding selection of spatial predictors
    if(inherits(model, "rf_spatial")){

      model.tuned$spatial <- spatial

    }

    return(model.tuned)

    #tuned model worse than original one
  } else {

    if(verbose == TRUE){
      message(
        paste0(
          "The tuned model (",
          metric.name,
          " = ",
          new.performance,
          ") performs worse than the original one (",
          metric.name,
          " = ",
          old.performance,
          "). Tuning not required, returning the original model."
        )
      )
    }

    #adding tuning slot
    model$tuning <- tuning.list

    #adding plot to the tunning slot
    model$tuning$plot <- plot_tuning(
      model,
      verbose = FALSE
    )

    #adding r squared gain
    model$tuning$performance.gain <- performance.gain

    if(verbose == TRUE){
      message("Tuning results stored in model$tuning.")
    }

    return(model)

  }

}
