#' @title Tuning of random forest hyperparameters via spatial cross-validation
#' @description Finds the optimal set of random forest hyperparameters `num.trees`, `mtry`, and `min.node.size` via grid search by maximizing the model's R squared, or AUC, if the response variable is binomial, via spatial cross-validation performed with [rf_evaluate()].
#' @param model A model fitted with [rf()]. If provided, the training data is taken directly from the model definition (stored in `model$ranger.arguments`). Default: `NULL`
#' @param num.trees Numeric integer vector with the number of trees to fit on each model repetition. Default: `c(500, 1000, 2000)`.
#' @param mtry Numeric integer vector, number of predictors to randomly select from the complete pool of predictors on each tree split. Default: `floor(seq(1, length(predictor.variable.names), length.out = 4))`
#' @param min.node.size Numeric integer, minimal number of cases in a terminal node. Default: `c(5, 10, 20, 40)`
#' @param xy Data frame or matrix with two columns containing coordinates and named "x" and "y". If `NULL`, the function will throw an error. Default: `NULL`
#' @param repetitions Integer, number of independent spatial folds to use during the cross-validation. Default: `30`.
#' @param training.fraction Proportion between 0.2 and 0.9 indicating the number of records to be used in model training. Default: `0.75`
#' @param seed Integer, random seed to facilitate reproduciblity. If set to a given number, the results of the function are always the same. Default: `1`.
#' @param verbose Logical. If TRUE, messages and plots generated during the execution of the function are displayed, Default: `TRUE`
#' @param n.cores Integer, number of cores to use for parallel execution. Creates a socket cluster with `parallel::makeCluster()`, runs operations in parallel with `foreach` and `%dopar%`, and stops the cluster with `parallel::clusterStop()` when the job is done. Default: `parallel::detectCores() - 1`
#' @param cluster A cluster definition generated with `parallel::makeCluster()`. If provided, overrides `n.cores`. When `cluster = NULL` (default value), and `model` is provided, the cluster in `model`, if any, is used instead. If this cluster is `NULL`, then the function uses `n.cores` instead. The function does not stop a provided cluster, so it should be stopped with `parallel::stopCluster()` afterwards. The cluster definition is stored in the output list under the name "cluster" so it can be passed to other functions via the `model` argument, or using the `%>%` pipe. Default: `NULL`
#' @return A model with a new slot named `tuning`, with a data frame with the results of the tuning analysis.
#' @seealso [rf_evaluate()]
#' @examples
#' if(interactive()){
#'
#' #loading example data
#' data(plant_richness_df)
#' data(distance_matrix)
#'
#' #fitting model to tune
#' out <- rf(
#'   data = plant_richness_df,
#'   dependent.variable.name = "richness_species_vascular",
#'   predictor.variable.names = colnames(plant_richness_df)[5:21],
#'   distance.matrix = distance_matrix,
#'   distance.thresholds = 0,
#'   n.cores = 1
#' )
#'
#' #model tuning
#' tuning <- rf_tuning(
#'   model = out,
#'   num.trees = c(100, 500),
#'   mtry = c(2, 8),
#'   min.node.size = c(5, 10),
#'   xy = plant_richness_df[, c("x", "y")],
#'   n.cores = 1
#' )
#'
#' }
#' @importFrom rlang sym
#' @rdname rf_tuning
#' @export
rf_tuning <- function(
  model = NULL,
  num.trees = NULL,
  mtry = NULL,
  min.node.size = NULL,
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

  if(is.null(model)){

    stop("The argument 'model' is empty, there is no model to tune.")
  } else {

    #getting arguments from model rather than ranger.arguments
    ranger.arguments <- model$ranger.arguments
    data <- ranger.arguments$data
    dependent.variable.name <- ranger.arguments$dependent.variable.name
    predictor.variable.names <- ranger.arguments$predictor.variable.names
    distance.matrix <- ranger.arguments$distance.matrix
    distance.thresholds <- ranger.arguments$distance.thresholds

    #getting cluster from model if "cluster" is not provided
    if(is.null(cluster) & "cluster" %in% names(model)){
      cluster <- model$cluster
    }

    #getting xy
    if(is.null(xy)){
      if(is.null(model$ranger.arguments$xy)){
        stop("The argument 'xy' is required for spatial cross-validation.")
      } else {
        xy <- model$ranger.arguments$xy
      }
    }
  }
  if(sum(c("x", "y") %in% colnames(xy)) < 2){
    stop("The column names of 'xy' must be 'x' and 'y'.")
  }
  if(nrow(xy) != nrow(data)){
    stop("nrow(xy) and nrow(data) (stored in model$ranger.arguments$data) must be the same.")
  }

  #saving model class for later
  model.class <- class(model)

  #testing if the data is binary
  is.binary <- is_binary(
    data = data,
    dependent.variable.name = dependent.variable.name
  )
  if(is.binary == TRUE){
    metric <- "auc"
  } else {
    metric <- "r.squared"
  }

  #saving slots if it's an rf_spatial model
  if(inherits(model, "rf_spatial")){
    spatial <- model$spatial
  }

  #mtry
  if(is.null(mtry)){
    mtry <- as.integer(seq(1, length(predictor.variable.names) - 1, length.out = 4))
  } else {
    if(max(mtry) > length(predictor.variable.names)){
      if(verbose == TRUE){
        message("Maximum 'mtry' set to length(predictor.variable.names)")
      }
      mtry <- mtry[mtry < length(predictor.variable.names)]
    }
  }
  mtry <- as.integer(mtry)

  #min.node.size
  if(is.null(min.node.size)){
    min.node.size <- c(5, 10, 20, 40)
  } else {
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
  }
  min.node.size <- as.integer(min.node.size)

  #num.trees
  if(is.null(num.trees)){
    num.trees <- c(500, 1000, 2000)
  }
  num.trees <- as.integer(num.trees)

  #combining values
  combinations <- expand.grid(
    num.trees = num.trees,
    mtry = mtry,
    min.node.size = min.node.size
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

  #CLUSTER SETUP
  #cluster is provided
  if(!is.null(cluster)){

    #n.cores <- NULL
    n.cores <- NULL

    #flat to not stop cluster after execution
    stop.cluster <- FALSE

  } else {

    #creates and registers cluster
    cluster <- parallel::makeCluster(
      n.cores,
      type = "PSOCK"
    )

    #flag to stop cluster
    stop.cluster <- TRUE

  }

  #registering cluster
  doParallel::registerDoParallel(cl = cluster)

  #looping through combinations
  tuning <- foreach::foreach(
    num.trees.i = combinations$num.trees,
    mtry.i = combinations$mtry,
    min.node.size.i = combinations$min.node.size,
    .combine = "rbind",
    .verbose = FALSE
  ) %dopar% {

    #filling ranger arguments
    if(!is.null(ranger.arguments)){
      ranger.arguments.i <- ranger.arguments
    } else {
      ranger.arguments.i <- list()
    }
    ranger.arguments.i$num.trees <- num.trees.i
    ranger.arguments.i$mtry <- mtry.i
    ranger.arguments.i$min.node.size <- min.node.size.i
    ranger.arguments.i$importance <- "none"
    ranger.arguments.i$num.threads <- 1
    ranger.arguments.i$save.memory <- TRUE

    #fit model with new hyperparameters
    m.i <- spatialRF::rf(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments.i,
      scaled.importance = FALSE,
      seed = seed,
      n.cores = 1,
      verbose = FALSE
    )

    #evaluate with spatial cross-validation
    m.i <- spatialRF::rf_evaluate(
      model = m.i,
      xy = xy,
      repetitions = repetitions,
      training.fraction = training.fraction,
      metrics = metric,
      seed = seed,
      verbose = FALSE,
      n.cores = 1
    )

    #getting performance measures
    m.i.performance <- spatialRF::get_evaluation(m.i)
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
  metric.name <- colnames(tuning)[!(colnames(tuning) %in% c("num.trees", "mtry", "min.node.size", "moran.i.interpretation"))]

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
    verbose = FALSE
  )

  #keeping class
  class(model.tuned) <- unique(c(class(model.tuned), model.class))

  #adding tuning slot
  model.tuned$tuning <- tuning.list

  #adding the variable importance slot if rf_spatial
  if(inherits(model, "rf_spatial")){

    model.tuned$importance <- prepare_importance_spatial(model = model.tuned)

  }

  #comparing metric of the old model and the tuned one

  #evaluate model
  model <- spatialRF::rf_evaluate(
    model = model,
    xy = xy,
    repetitions = repetitions,
    training.fraction = training.fraction,
    seed = seed,
    verbose = FALSE,
    n.cores = n.cores,
    cluster = cluster
  )

  #evaluate model tuned
  model.tuned <- spatialRF::rf_evaluate(
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
  new.performance <- round(
    model.tuned$evaluation$aggregated[model.tuned$evaluation$aggregated$model == "Testing" &
        model.tuned$evaluation$aggregated$metric == metric.name,
      "median"
    ],
    3
  )
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

    #stopping cluster
    if(stop.cluster == TRUE){
      parallel::stopCluster(cl = cluster)
    } else {
      model$cluster <- cluster
    }

    if(verbose == TRUE){
      message("Tuning results stored in model$tuning.")
    }

    return(model)

  }

}
