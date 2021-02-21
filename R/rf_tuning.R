#' @title Tuning of random forest hyperparameters
#' @description Tunes the random forest hyperparameters `num.trees`, `mtry`, and `min.node.size` via grid search by maximizing the model's R squared. Two methods are available: out-of-bag (`oob`), and spatial cross-validation performed with [rf_evaluate()].
#' @param model A model fitted with [rf()]. If provided, the training data is taken directly from the model definition (stored in `model$ranger.arguments`). Default: `NULL`
#' @param data Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param dependent.variable.name Character string with the name of the response variable. Must be in the column names of `data`. Default: `NULL`
#' @param predictor.variable.names Character vector with the names of the predictive variables, or output of [auto_cor()] or [auto_vif()]. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param method Character, "oob" to use RMSE values computed on the out-of-bag data to guide the tuning, and "spatial.cv", to use RMSE values from a spatial cross-validation on independent spatial folds done via [rf_evaluate()]. Default: `"oob"`
#' @param num.trees Numeric integer vector with the number of trees to fit on each model repetition. Default: c(500, 1000).
#' @param mtry Numeric integer vector, number of predictors to randomly select from the complete pool of predictors on each tree split. Default: `c(2, 3)`
#' @param min.node.size Numeric integer, minimal number of cases in a terminal node. Default: `c(5, 10)`
#' @param xy Data frame or matrix with two columns containing coordinates and named "x" and "y", or an sf file with geometry class `sfc_POINT` (see [plant_richness_sf]). If `NULL`, the function will throw an error. Default: `NULL`
#' @param repetitions Integer, number of repetitions to compute the R squared from. If `method = "oob"`, number of repetitions to be used in [rf_repeat()] to fit models for each combination of hyperparameters. If `method = "spatial.cv"`, number of independent spatial folds to use during the cross-validation. Default: `NULL` (which yields 30 for "spatial.cv" and 5 for "oob").
#' @param training.fraction Proportion between 0.2 and 0.8 indicating the number of records to be used in model training. Default: `0.6`
#' @param verbose Logical. If TRUE, messages and plots generated during the execution of the function are displayed, Default: `TRUE`
#' @param n.cores Integer, number of cores to use during computations. If `NULL`, all cores but one are used, unless a cluster is used. Default = `NULL`
#' @param cluster.ips Character vector with the IPs of the machines in a cluster. The machine with the first IP will be considered the main node of the cluster, and will generally be the machine on which the R code is being executed.
#' @param cluster.cores Numeric integer vector, number of cores to use on each machine.
#' @param cluster.user Character string, name of the user (should be the same throughout machines). Defaults to the current system user.
#' @param cluster.port Integer, port used by the machines in the cluster to communicate. The firewall in all computers must allow traffic from and to such port. Default: `11000`
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
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  method = c("oob", "spatial.cv"),
  num.trees = c(500, 1000),
  mtry = c(1, 5),
  min.node.size = c(5, 10),
  xy = NULL,
  repetitions = NULL,
  training.fraction = 0.6,
  verbose = TRUE,
  n.cores = NULL,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = Sys.info()[["user"]],
  cluster.port = 11000
){

  #declaring variables
  value <- NULL
  r.squared <- NULL
  num.trees.i <- NULL
  mtry.i <- NULL
  min.node.size.i <- NULL

  #matching arguments
  method <- match.arg(
    method,
    choices = c("oob", "spatial.cv")
  )

  if(is.null(repetitions)){
    if(method == "oob"){
      repetitions <- 5
    }
    if(method == "spatial.cv"){
      repetitions <- 30
    }
  }

  #getting arguments from model rather than ranger.arguments
  if(!is.null(model)){
    ranger.arguments <- model$ranger.arguments
    data <- ranger.arguments$data
    dependent.variable.name <- ranger.arguments$dependent.variable.name
    predictor.variable.names <- ranger.arguments$predictor.variable.names
    distance.matrix <- ranger.arguments$distance.matrix
    distance.thresholds <- ranger.arguments$distance.thresholds
    scaled.importance <- ranger.arguments$scaled.importance
  } else {
    distance.matrix <- NULL
    distance.thresholds <- NULL
  }

  #mtry
  if(max(mtry) > length(predictor.variable.names)){
    mtry[mtry == max(mtry)] <- length(predictor.variable.names)
  }
  if(is.null(mtry)){
    mtry <- floor(sqrt(length(predictor.variable.names)))
  }

  #min.node.size
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
  if(is.null(min.node.size)){
    min.node.size <- 5
  }

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

    #using out of bag
    if(method == "oob"){

      #fit model
      m.i <- spatialRF::rf_repeat(
        data = data,
        dependent.variable.name = dependent.variable.name,
        predictor.variable.names = predictor.variable.names,
        ranger.arguments = ranger.arguments.i,
        scaled.importance = FALSE,
        verbose = FALSE,
        repetitions = repetitions,
        n.cores = 1
      )

      #get performance measures
      m.i.performance <- spatialRF::get_performance(m.i)[, 1:2]

    }

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

    }

    #gathering into data frame
    m.i.performance <- data.frame(
      r.squared = m.i.performance[m.i.performance$metric == "r.squared", "mean"]
    )

    return(m.i.performance)

  }#end of loop

  #binding with combinations
  tuning <- cbind(
    combinations,
    tuning
  ) %>%
    dplyr::arrange(dplyr::desc(r.squared))

  #preparing ranger arguments
  ranger.arguments <- list()
  ranger.arguments$num.trees <- tuning[1, "num.trees"]
  ranger.arguments$mtry <- tuning[1, "mtry"]
  ranger.arguments$min.node.size <- tuning[1, "min.node.size"]

  #fitting tuned model
  m <- rf(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    ranger.arguments = ranger.arguments,
    distance.matrix = distance.matrix,
    distance.thresholds = distance.thresholds,
    verbose = FALSE
    )

  #preparing tuning list
  tuning.list <- list()
  tuning.list$method <- method
  tuning.list$tuning.df <- tuning

  #adding tuning slot
  m$tuning <- tuning.list

  #adding plot to the tunning slot
  m$tuning$plot <- plot_tuning(
    x = m,
    verbose = FALSE
    )

  #message and plot
  if(verbose == TRUE){
    message("Best hyperparameters:")
    message(paste0("  - num.trees:     ", tuning[1, "num.trees"]))
    message(paste0("  - mtry:          ", tuning[1, "mtry"]))
    message(paste0("  - min.node.size: ", tuning[1, "min.node.size"]))
    plot_tuning(m)
  }

  #returning output
  m

}
