#' @title Tuning of random forest hyperparameters via spatial cross-validation
#' @description Finds the optimal set of random forest hyperparameters `num.trees`, `mtry`, and `min.node.size` via grid search by maximizing the model's R squared, or AUC, if the response variable is binomial, via spatial cross-validation performed with [rf_evaluate()].
#' @param model A model fitted with [rf()]. If provided, the training data is taken directly from the model definition (stored in `model$ranger.arguments`). Default: `NULL`
#' @param num.trees Numeric integer vector with the number of trees to fit on each model repetition. Default: `c(500, 1000, 2000)`.
#' @param mtry Numeric integer vector, number of predictors to randomly select from the complete pool of predictors on each tree split. Default: `floor(seq(1, length(predictor.variable.names), length.out = 4))`
#' @param min.node.size Numeric integer, minimal number of cases in a terminal node. Default: `c(5, 10, 20, 40)`
#' @param xy Data frame or matrix with two columns containing coordinates and named "x" and "y". If `NULL`, the function will throw an error. Default: `NULL`
#' @param repetitions Integer, number of independent spatial folds to use during the cross-validation. Default: `30`.
#' @param training.fraction Proportion between 0.2 and 0.9 indicating the number of records to be used in model training. Default: `0.75`
#' @param seed Integer, random seed to facilitate reproduciblity. If set to a given number, the results of the function are always the same.
#' @param verbose Logical. If TRUE, messages and plots generated during the execution of the function are displayed, Default: `TRUE`
#' @param n.cores Integer, number of cores to use. Default = `parallel::detectCores() - 1`
#' @param cluster.ips Character vector with the IPs of the machines in a cluster. The machine with the first IP will be considered the main node of the cluster, and will generally be the machine on which the R code is being executed.
#' @param cluster.cores Numeric integer vector, number of cores to use on each machine.
#' @param cluster.user Character string, name of the user (should be the same throughout machines). Defaults to the current system user.
#' @param cluster.port Character, port used by the machines in the cluster to communicate. The firewall in all computers must allow traffic from and to such port. Default: `"11000"`
#' @return A list with four slots: `tuning` data frame with the results of the tuning analysis; `tuning.long`, a long version of the previous data frame; `tuning.plot`, ggplot of `tuning.long`; `ranger.arguments`, a list ready to be used as the argument `ranger.arguments` in [rf_repeat()] or [rf_spatial()].
#' @seealso [rf_evaluate()]
#' @examples
#' \donttest{
#' if(interactive()){
#'
#' data(plant_richness_df)
#' data(distance_matrix)
#'
#' #fitting model to tune
#' out <- rf(
#'   data = plant_richness_df,
#'   dependent.variable.name = "richness_species_vascular",
#'   predictor.variable.names = colnames(plant_richness_df)[5:21],
#'   distance.matrix = distance_matrix,
#'   distance.thresholds = c(0, 100, 1000, 10000)
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
  seed = NULL,
  verbose = TRUE,
  n.cores = parallel::detectCores() - 1,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = Sys.info()[["user"]],
  cluster.port = "11000"
){

  #declaring variables
  metric <- NULL
  num.trees.i <- NULL
  mtry.i <- NULL
  min.node.size.i <- NULL

  if(is.null(model)){
    stop("The argument 'model' is empty, there is no model to tune.")
  }

  if(is.null(xy)){
    stop("The argument 'xy' is required for spatial cross-validation.")
  }

  #getting arguments from model rather than ranger.arguments
  ranger.arguments <- model$ranger.arguments
  data <- ranger.arguments$data
  dependent.variable.name <- ranger.arguments$dependent.variable.name
  predictor.variable.names <- ranger.arguments$predictor.variable.names
  distance.matrix <- ranger.arguments$distance.matrix
  distance.thresholds <- ranger.arguments$distance.thresholds
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
    selection.spatial.predictors <- model$selection.spatial.predictors
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
  #if no cluster.ips, local cluster
  if(is.null(cluster.ips)){

    #sequential execution
    if(n.cores == 1){

      #replaces dopar (parallel) by do (serial)
      `%dopar%` <- foreach::`%do%`
      on.exit(`%dopar%` <- foreach::`%dopar%`)

      #sets other cluster values to NULL
      cluster.ips <- NULL

      #parallel execution
    } else {

      #creates and registers cluster
      temp.cluster <- parallel::makeCluster(
        n.cores,
        type = "PSOCK"
      )

      #sets dopar in case it was not setup
      `%dopar%` <- foreach::`%dopar%`

    }

    #beowulf cluster
  } else {

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

  }

  #registering cluster if it exists
  if(exists("temp.cluster")){
    doParallel::registerDoParallel(cl = temp.cluster)
  }


  #looping through combinations
  tuning <- foreach::foreach(
    num.trees.i = combinations$num.trees,
    mtry.i = combinations$mtry,
    min.node.size.i = combinations$min.node.size,
    .combine = "rbind",
    .verbose = verbose
  ) %dopar% {

    #filling ranger arguments
    ranger.arguments.i <- list()
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


  if(exists("temp.cluster")){
    parallel::stopCluster(cl = temp.cluster)
  }

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

  #fitting tuned model with rf
  if(!inherits(model, "rf_repeat")){

    model.tuned <- rf(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      ranger.arguments = ranger.arguments,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      verbose = FALSE
    )

  }

  #fitting tuned model with rf repeat
  if(inherits(model, "rf_repeat")){

    model.tuned <- rf_repeat(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      ranger.arguments = ranger.arguments,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      repetitions = model$ranger.arguments$repetitions,
      verbose = FALSE,
      n.cores = n.cores,
      cluster.ips = cluster.ips,
      cluster.cores = cluster.cores,
      cluster.user = cluster.user,
      cluster.port = cluster.port
    )

  }

  #keeping class
  class(model.tuned) <- unique(c(class(model.tuned), model.class))

  #adding tuning slot
  model.tuned$tuning <- tuning.list

  #adding the variable importance slot if rf_spatial
  if(inherits(model, "rf_spatial")){

    model.tuned$variable.importance <- prepare_importance_spatial(model.tuned)

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
    cluster.ips = cluster.ips,
    cluster.cores = cluster.cores,
    cluster.user = cluster.user,
    cluster.port = cluster.port
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
    cluster.ips = cluster.ips,
    cluster.cores = cluster.cores,
    cluster.user = cluster.user,
    cluster.port = cluster.port
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
      plot_tuning(model.tuned)
      message("Best hyperparameters:")
      message(paste0("  - num.trees:     ", tuning[1, "num.trees"]))
      message(paste0("  - mtry:          ", tuning[1, "mtry"]))
      message(paste0("  - min.node.size: ", tuning[1, "min.node.size"]))
      message(paste0(
        "gain in ",
        metric.name,
        ": ",
        performance.gain
      )
      )
    }

    #adding gain
    model.tuned$tuning$performance.gain <- performance.gain

    #adding selection of spatial predictors
    if(inherits(model, "rf_spatial")){

      model.tuned$selection.spatial.predictors <- selection.spatial.predictors

    }

    return(model.tuned)

    #tuned model worse than original one
  } else {

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

    #adding tuning slot
    model$tuning <- tuning.list

    #adding plot to the tunning slot
    model$tuning$plot <- plot_tuning(
      model,
      verbose = FALSE
    )

    #adding r squared gain
    model$tuning$performance.gain <- performance.gain

    return(model)

  }

}
