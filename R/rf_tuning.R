#' @title Tuning of random forest hyperparameters
#' @description Tunes the random forest hyperparameters `num.trees`, `mtry`, and `min.node.size` via grid search by maximizing the model's R squared. Two methods are available: out-of-bag (`oob`), and spatial cross-validation performed with [rf_evaluate()].
#' @param model A model fitted with [rf()]. If provided, the arguments `data`, `dependent.variable.name`, `predictor.variable.names`, `distance.matrix`, `distance.thresholds`, `ranger.arguments`, and `scaled.importance` are taken directly from the model definition (stored in `model$ranger.arguments`). Default: `NULL`
#' @param data Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param dependent.variable.name Character string with the name of the response variable. Must be in the column names of `data`. Default: `NULL`
#' @param predictor.variable.names Character vector with the names of the predictive variables, or output of [auto_cor()] or [auto_vif()]. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param method Character, "oob" to use RMSE values computed on the out-of-bag data to guide the tuning, and "spatial.cv", to use RMSE values from a spatial cross-validation on independent spatial folds done via [rf_evaluate()]. Default: `"oob"`
#' @param num.trees Numeric integer vector with the number of trees to fit on each model repetition. Defalut: c(500, 1000).
#' @param mtry Numeric integer vector, number of predictors to randomly select from the complete pool of predictors on each tree split. Default: `c(2, 3)`
#' @param min.node.size Numeric integer, minimal number of cases in a terminal node. Default: `c(5, 10)`
#' @param xy Data frame or matrix with two columns containing coordinates and named "x" and "y", or an sf file with geometry class `sfc_POINT` (see [plant_richness_sf]). If `NULL`, the function will throw an error. Default: `NULL`
#' @param repetitions Integer, number of repetitions to compute the R squared from. If `method = "oob"`, number of repetitions to be used in [rf_repeat()] to fit models for each combination of hyperparameters. If `method = "spatial.cv"`, number of independent spatial folds to use during the cross-validation. Default: `10`
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
#' \dontrun{
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
  method = "oob",
  num.trees = c(500, 1000),
  mtry = c(1, 5),
  min.node.size = c(5, 10),
  xy = NULL,
  repetitions = 10,
  training.fraction = 0.6,
  verbose = TRUE,
  n.cores = NULL,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = NULL,
  cluster.port = 11000
){

  #declaring variables
  rmse <- NULL
  value <- NULL

  #getting arguments from model rather than ranger.arguments
  if(!is.null(model)){
    ranger.arguments <- model$ranger.arguments
    data <- ranger.arguments$data
    dependent.variable.name <- ranger.arguments$dependent.variable.name
    predictor.variable.names <- ranger.arguments$predictor.variable.names
    distance.matrix <- ranger.arguments$distance.matrix
    distance.thresholds <- ranger.arguments$distance.thresholds
    scaled.importance <- ranger.arguments$scaled.importance
  }

  #mtry
  if(max(mtry) > length(predictor.variable.names)){
    if(verbose == TRUE){
      message(paste0(
        "'mtry' values larger than the number of predictors (",
        length(predictor.variable.names),
        ") were removed."
      )
      )
    }
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

  #copy of ranger.arguments for iterations
  ranger.arguments.i <- ranger.arguments

  #looping through combinations
  tuning <- foreach(
    num.trees = combinations$num.trees,
    mtry = combinations$mtry,
    min.node.size = combinations$min.node.size,
    .combine = "rbind"
  ) %do% {

    #filling ranger arguments
    ranger.arguments.i$num.trees <- num.trees
    ranger.arguments.i$mtry <- mtry
    ranger.arguments.i$min.node.size <- min.node.size
    ranger.arguments.i$importance <- "none"
    ranger.arguments.i$data = data
    ranger.arguments.i$dependent.variable.name = dependent.variable.name
    ranger.arguments.i$predictor.variable.names = predictor.variable.names

    #using out of bag
    if(method == "oob"){

      #fit model
      m.i <- rf_repeat(
        ranger.arguments = ranger.arguments.i,
        scaled.importance = FALSE,
        repetitions = repetitions,
        verbose = FALSE,
        n.cores = n.cores,
        cluster.ips = cluster.ips,
        cluster.cores = cluster.cores,
        cluster.user = cluster.user,
        cluster.port = cluster.port
      )

      #get performance measures
      m.i.performance <- get_performance(m.i)[, 1:2]

    }

    #using rf_evaluate
    if(method == "spatial.cv"){

      #fit model
      m.i <- rf(
        ranger.arguments = ranger.arguments.i,
        scaled.importance = FALSE,
        seed = 100,
        verbose = FALSE
      )

      #evaluate
      m.i <- rf_evaluate(
        model = m.i,
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

      #getting performance measures
      m.i.performance <- get_evaluation(m.i)
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
    dplyr::arrange(desc(r.squared))

  #preparing ranger arguments
  ranger.arguments <- list()
  ranger.arguments$data <- data
  ranger.arguments$dependent.variable.name <- dependent.variable.name
  ranger.arguments$predictor.variable.names <- predictor.variable.names
  ranger.arguments$num.trees <- tuning[1, "num.trees"]
  ranger.arguments$mtry <- tuning[1, "mtry"]
  ranger.arguments$min.node.size <- tuning[1, "min.node.size"]

  #preparing tuning list
  tuning.list <- list()
  tuning.list$method <- method
  tuning.list$tuning.df <- tuning

  #fitting tuned model
  m <- rf(
    ranger.arguments = ranger.arguments,
    distance.matrix = distance.matrix,
    distance.thresholds = distance.thresholds,
    verbose = FALSE
    )

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