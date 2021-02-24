#' @title Tuning of random forest hyperparameters
#' @description Tunes the random forest hyperparameters `num.trees`, `mtry`, and `min.node.size` via grid search by maximizing the model's R squared. Two methods are available: out-of-bag (`oob`), and spatial cross-validation performed with [rf_evaluate()].
#' @param model A model fitted with [rf()]. If provided, the training data is taken directly from the model definition (stored in `model$ranger.arguments`). Default: `NULL`
#' @param method Character, "oob" to use RMSE values computed on the out-of-bag data to guide the tuning, and "spatial.cv", to use RMSE values from a spatial cross-validation on independent spatial folds done via [rf_evaluate()]. Default: `"oob"`
#' @param num.trees Numeric integer vector with the number of trees to fit on each model repetition. Default: `c(500, 1000, 2000)`.
#' @param mtry Numeric integer vector, number of predictors to randomly select from the complete pool of predictors on each tree split. Default: `floor(seq(1, length(predictor.variable.names), length.out = 4))`
#' @param min.node.size Numeric integer, minimal number of cases in a terminal node. Default: `c(5, 10, 20, 40)`
#' @param xy Data frame or matrix with two columns containing coordinates and named "x" and "y". If `NULL`, the function will throw an error. Default: `NULL`
#' @param repetitions Integer, number of repetitions to compute the R squared from. If `method = "oob"`, number of repetitions to be used in [rf_repeat()] to fit models for each combination of hyperparameters. If `method = "spatial.cv"`, number of independent spatial folds to use during the cross-validation. Default: `NULL` (which yields 30 for "spatial.cv" and 5 for "oob").
#' @param training.fraction Proportion between 0.2 and 0.8 indicating the number of records to be used in model training. Default: `0.8`
#' @param verbose Logical. If TRUE, messages and plots generated during the execution of the function are displayed, Default: `TRUE`
#' @param n.cores Integer, number of cores to use during computations. If `NULL`, all cores but one are used, unless a cluster is used. Default = `NULL`
#' @param cluster.ips Character vector with the IPs of the machines in a cluster. The machine with the first IP will be considered the main node of the cluster, and will generally be the machine on which the R code is being executed.
#' @param cluster.cores Numeric integer vector, number of cores to use on each machine.
#' @param cluster.user Character string, name of the user (should be the same throughout machines). Defaults to the current system user.
#' @param cluster.port Character, port used by the machines in the cluster to communicate. The firewall in all computers must allow traffic from and to such port. Default: `"11000"`
#' @return A list with four slots: `tuning` data frame with the results of the tuning analysis; `tuning.long`, a long version of the previous data frame; `tuning.plot`, ggplot of `tuning.long`; `ranger.arguments`, a list ready to be used as the argument `ranger.arguments` in [rf_repeat()] or [rf_spatial()].
#' @details The tuning method "oob" uses as reference the RMSE computed on the out-of-bag data, while the method "spatial.cv" uses RMSE computed on spatially independent data unseen by the model. The RMSE values of the latter method will always be higher, but inform better about the capacity of the combinations of hyperparameters to yield more general models.
#' @examples
#' \donttest{
#' if(interactive()){
#'
#' data(plant_richness_df)
#' data(distance_matrix)
#'
#' tuning <- rf_tuning(
#'   data = plant_richness_df,
#'   dependent.variable.name = "richness_species_vascular",
#'   predictor.variable.names = colnames(plant_richness_df)[5:21],
#'   method = "oob",
#'   n.cores = 1,
#'   verbose = FALSE
#' )
#'
#' }
#' }
#' @rdname rf_tuning
#' @export
rf_tuning <- function(
  model = NULL,
  method = c("oob", "spatial.cv"),
  num.trees = NULL,
  mtry = NULL,
  min.node.size = NULL,
  xy = NULL,
  repetitions = NULL,
  training.fraction = 0.8,
  verbose = TRUE,
  n.cores = NULL,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = Sys.info()[["user"]],
  cluster.port = "11000"
){

  #declaring variables
  r.squared <- NULL
  num.trees.i <- NULL
  mtry.i <- NULL
  min.node.size.i <- NULL

  #matching arguments
  method <- match.arg(
    method,
    choices = c("oob", "spatial.cv")
  )

  if(is.null(model)){
    stop("The argument 'model' is empty, there is no model to tune.")
  }

  if(is.null(repetitions)){
    if(method == "spatial.cv"){
      repetitions <- 30
    }
  }

  #getting arguments from model rather than ranger.arguments
  ranger.arguments <- model$ranger.arguments
  data <- ranger.arguments$data
  dependent.variable.name <- ranger.arguments$dependent.variable.name
  predictor.variable.names <- ranger.arguments$predictor.variable.names
  distance.matrix <- ranger.arguments$distance.matrix
  distance.thresholds <- ranger.arguments$distance.thresholds
  model.class <- class(model)

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

    if(.Platform$OS.type == "windows"){
      temp.cluster <- parallel::makeCluster(
        n.cores,
        type = "PSOCK"
      )
    } else {
      temp.cluster <- parallel::makeCluster(
        n.cores,
        type = "FORK"
      )
    }

    #register cluster and close on exit
    doParallel::registerDoParallel(cl = temp.cluster)
    on.exit(parallel::stopCluster(cl = temp.cluster))

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
    on.exit(parallel::stopCluster(cl = temp.cluster))

  }

  #looping through combinations
  tuning <- foreach(
    num.trees.i = combinations$num.trees,
    mtry.i = combinations$mtry,
    min.node.size.i = combinations$min.node.size,
    .combine = "rbind"
  ) %dopar% {

    #filling ranger arguments
    ranger.arguments.i <- list()
    ranger.arguments.i$num.trees <- num.trees.i
    ranger.arguments.i$mtry <- mtry.i
    ranger.arguments.i$min.node.size <- min.node.size.i
    ranger.arguments.i$importance <- "none"
    ranger.arguments.i$num.threads <- 1

    #computing Moran's I if the model is rf_spatial
    if(inherits(model, "rf_spatial")){

      #fit model
      m.i <- spatialRF::rf_repeat(
        data = data,
        dependent.variable.name = dependent.variable.name,
        predictor.variable.names = predictor.variable.names,
        distance.matrix = distance.matrix,
        distance.thresholds = distance.thresholds,
        ranger.arguments = ranger.arguments.i,
        scaled.importance = FALSE,
        verbose = FALSE,
        repetitions = 10,
        n.cores = 1
      )

      moran.i.interpretation <- m.i$spatial.correlation.residuals$per.distance$interpretation[1]

    }

    #using out of bag
    if(method == "oob"){

      #do not run if the model is already fitted
      if(!inherits(model, "rf_spatial")){

        #fit model
        m.i <- spatialRF::rf_repeat(
          data = data,
          dependent.variable.name = dependent.variable.name,
          predictor.variable.names = predictor.variable.names,
          ranger.arguments = ranger.arguments.i,
          scaled.importance = FALSE,
          verbose = FALSE,
          repetitions = 10,
          n.cores = 1
        )

      }

      #get performance measures
      m.i.performance <- spatialRF::get_performance(m.i)[, 1:2]

    }#end of oob

    #using rf_evaluate
    if(method == "spatial.cv"){

      #fit model
      m.i <- spatialRF::rf(
        data = data,
        dependent.variable.name = dependent.variable.name,
        predictor.variable.names = predictor.variable.names,
        ranger.arguments = ranger.arguments.i,
        scaled.importance = FALSE,
        seed = 100,
        verbose = FALSE
      )

      #evaluate
      m.i <- spatialRF::rf_evaluate(
        model = m.i,
        xy = xy,
        repetitions = repetitions,
        training.fraction = training.fraction,
        verbose = FALSE,
        n.cores = 1
      )

      #getting performance measures
      m.i.performance <- spatialRF::get_evaluation(m.i)
      m.i.performance <- m.i.performance[m.i.performance$model == "Testing", c("metric", "mean")]

    }#end of spatial.cv

    #gathering into data frame
    if(inherits(model, "rf_spatial")){
      m.i.performance <- data.frame(
        r.squared = m.i.performance[m.i.performance$metric == "r.squared", "mean"],
        moran.i.interpretation = moran.i.interpretation
      )
    } else {
      m.i.performance <- data.frame(
        r.squared = m.i.performance[m.i.performance$metric == "r.squared", "mean"]
      )
    }

    return(m.i.performance)

  }#end of loop

  #binding with combinations
  tuning <- cbind(
    combinations,
    tuning
  ) %>%
    dplyr::arrange(dplyr::desc(r.squared))

  #preparing tuning list
  tuning.list <- list()
  tuning.list$method <- method
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
       stop()
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

    repetitions <- model$ranger.arguments$repetitions
    if(is.null(repetitions)){repetitions <- 10}

    model.tuned <- rf_repeat(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      ranger.arguments = ranger.arguments,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      repetitions = repetitions,
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

  #adding plot to the tunning slot
  model.tuned$tuning$plot <- plot_tuning(
    x = model.tuned,
    verbose = FALSE
  )

  #adding the variable importance slot if rf_spatial
  if(inherits(model, "rf_spatial")){
    model.tuned$variable.importance <- prepare_importance_spatial(model.tuned)
  }

  #comparing r squared of model and tuning

  #if oob
  if(method == "oob"){
    tuning.r.squared <- model.tuned$performance$r.squared
    model.r.squared <- model$performance$r.squared
  }

  #if spatial cross-validation
  if(method == "spatial.cv"){

    #evaluate model
    model <- spatialRF::rf_evaluate(
      model = model,
      xy = xy,
      repetitions = repetitions,
      training.fraction = training.fraction,
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
      verbose = FALSE,
      n.cores = n.cores,
      cluster.ips = cluster.ips,
      cluster.cores = cluster.cores,
      cluster.user = cluster.user,
      cluster.port = cluster.port
    )

    #extract r.squared values
    tuning.r.squared <- round(
      model.tuned$evaluation$aggregated
      [model.tuned$evaluation$aggregated$model == "Testing" &
          model.tuned$evaluation$aggregated$metric == "r.squared",
        "mean"
        ],
      3
      )
    model.r.squared <- round(
      model$evaluation$aggregated[
        model$evaluation$aggregated$model == "Testing" &
          model$evaluation$aggregated$metric == "r.squared",
        "mean"
        ],
      3
      )
  }

  #if there is r-squared gain
  if(tuning.r.squared > model.r.squared){

    #plot tuning
    if(verbose == TRUE){
      plot_tuning(model.tuned)
      message("Best hyperparameters:")
      message(paste0("  - num.trees:     ", tuning[1, "num.trees"]))
      message(paste0("  - mtry:          ", tuning[1, "mtry"]))
      message(paste0("  - min.node.size: ", tuning[1, "min.node.size"]))
      message(paste0("R squared gain: ", tuning.r.squared - model.r.squared))
    }

    #adding r squared gain
    model.tuned$tuning$r.squared.gain <- tuning.r.squared - model.r.squared

    #adding selection of spatial predictors
    if(inherits(model, "rf_spatial")){
      model.tuned$selection.spatial.predictors <- selection.spatial.predictors
    }

    return(model.tuned)

    #tuned model worse than original one
  } else {

    message(
      paste0(
        "The tuned model (R2: ",
        tuning.r.squared,
        ") performs worse than the original one (R2: ",
        model.r.squared,
        "). Tuning not required, returning the original model."
      )
    )

    #adding tuning slot
    model$tuning <- tuning.list

    #adding plot to the tunning slot
    model$tuning$plot <- plot_tuning(
      x = model,
      verbose = FALSE
    )

    #adding r squared gain
    model$tuning$r.squared.gain <- tuning.r.squared - model.r.squared

    return(model)

  }

}
