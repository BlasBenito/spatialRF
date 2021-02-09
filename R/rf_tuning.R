#' @title rf_tuning
#' @description Tunes the random forest hyperparameters `num.trees`, `mtry`, and `min.node.size`.
#' @param model Model to tune, produced by [rf()], [rf_repeat()], or [rf_spatial()].
#' @param data data frame with a response variable and a set of (preferably uncorrelated) predictors. Not required if `model` is provided. Default: NULL
#' @param dependent.variable.name string with the name of the response variable. Must be in the column names of 'data'. Not required if `model` is provided. Default: NULL
#' @param predictor.variable.names character vector with the names of the predictive variables. Every element must be in the column names of 'data'. Not required if `model` is provided. Default: NULL
#' @param ranger.arguments (optional) list with \link[ranger]{ranger} arguments. All \link[ranger]{ranger} arguments are set to their default values except for 'importance', that is set to 'permutation' rather than 'none'. Please, consult the help file of \link[ranger]{ranger} if you are not familiar with the arguments of this function.
#' @param tuning.method Character, "oob" to use R squared computed on the out-of-bag data to guide the tuning analysis, and "spatial.cv" to use R squared values from a spatial cross-validation done via [rf_evaluate()]. Default: "spatial.cv".
#' @param num.trees Numeric integer vector with the number of trees to fit on each model repetition. Defalut: c(500, 1000).
#' @param trees.per.variable (optional) number of individual regression trees to fit per variable in 'predictor.variable.names'. This is an alternative way to define ranger's 'num.trees'. If NULL, 'num.trees' is 500. Default: NULL
#' @param mtry Numeric integer vector, number of predictors to randomly select from the complete pool of predictors on each tree split. Default: c(2, 3)
#' @param min.node.size Numeric integer, minimal number of cases in a terminal node. Default:  c(5, 10)
#' @param xy Data frame or matrix with two columns containing coordinates and named "x" and "y", or an sf file with geometry class `sfc_POINT` (see [plant_richness_sf]). If `NULL`, the function will throw an error. Default: `NULL`
#' @param repetitions Integer, number of repetitions to compute the R squared from. If `tuning.method = "oob"`, number of repetitions to be used in [rf_repeat()] to fit models for each combination of hyperparameters. If `tuning.method = "spatial.cv"`, number of independent spatial folds to use during the cross-validation. Default: 10
#' @param training.fraction Proportion between 0.2 and 0.8 indicating the number of records to be used in model training. Default: 0.6
#' @param verbose Boolean. If TRUE, messages and plots generated during the execution of the function are displayed, Default: TRUE
#' @param n.cores number of cores to use to compute repetitions. If NULL, all cores but one are used, unless a cluster is used.
#' @param cluster.ips character vector, IPs of the machines in the cluster. The first machine will be considered the main node of the cluster, and will generally be the machine on which the R code is being executed.
#' @param cluster.cores numeric integer vector, number of cores on each machine.
#' @param cluster.user character string, name of the user (should be the same throughout machines), Defaults to the current system user. Default: user name of the current session.
#' @param cluster.port integer, port used by the machines in the cluster to communicate. The firewall in all computers must allow traffic from and to such port. Default: 11000.
#' @return A list with four slots: **tuning** data frame with the results of the tuning analysis; **tuning.long**, a long version of the previous data frame; **tuning.plot**, ggplot of **tuning.long**; **ranger.arguments**, a list ready to be used as the argument `ranger.arguments` in [rf_repeat()] or [rf_spatial()].
#' @details The tuning method "oob" uses as reference the R squared computed on the out-of-bag data, while the method "spatial.cv" uses R squared computed on spatially independent data unseen by the model. The R squared values of the latter method will always be lower, but inform better about the capacity of the combinations of hyperparameters to yield more general models.
#' @examples
#' \dontrun{
#' if(interactive()){
#' data(plant_richness_df)
#' data(distance_matrix)
#'
#' tuning <- rf_tuning(
#'   data = plant_richness_df,
#'   dependent.variable.name = "richness_species_vascular",
#'   predictor.variable.names = colnames(plant_richness_df)[5:21],
#'   n.cores = 1,
#'   verbose = FALSE
#' )
#'  }
#' }
#' @rdname rf_tuning
#' @export
rf_tuning <- function(
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  ranger.arguments = NULL,
  tuning.method = "spatial.cv",
  num.trees = c(500, 1000),
  trees.per.variable = NULL,
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

  #checking arguments to tune
  #num.trees vs. trees.per.variable
  if(!is.null(trees.per.variable) & !is.null(num.trees)){
    if(verbose == TRUE){
      message("'num.trees' and 'trees.per.variable' are provided, using 'num.trees' only.")
    }
  }
  if(!is.null(trees.per.variable) & is.null(num.trees)){
    num.trees <- trees.per.variable * length(predictor.variable.names)
  }
  if(is.null(trees.per.variable) & is.null(num.trees)){
    num.trees <- 500
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

  #copy of ranger arguments
  if(is.null(ranger.arguments)){
    ranger.arguments.i <- list()
  } else {
    ranger.arguments.i <- ranger.arguments
  }

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

    #using out of bag
    if(tuning.method == "oob"){

      #fit model
      m.i <- rf_repeat(
        data = data,
        dependent.variable.name = dependent.variable.name,
        predictor.variable.names = predictor.variable.names,
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
    if(tuning.method == "spatial.cv"){

      #fit model
      m.i <- rf(
        data = data,
        dependent.variable.name = dependent.variable.name,
        predictor.variable.names = predictor.variable.names,
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
      rmse = m.i.performance[m.i.performance$metric == "rmse", "mean"]
    )

    return(m.i.performance)

  }#end of loop

  #binding with combinations
  tuning <- cbind(
    combinations,
    tuning
  ) %>%
    dplyr::arrange(rmse)

  #to long format
  tuning.long <- tidyr::pivot_longer(
    tuning,
    cols = 1:3,
    names_to = "parameter",
    values_to = "value"
  ) %>%
    as.data.frame()

  p <- ggplot2::ggplot(
    data = tuning.long,
    ggplot2::aes(
      y = rmse,
      x = value,
      fill = rmse
    )) +
    ggplot2::geom_smooth(
      method = "lm",
      se = TRUE,
      color = "gray20",
      alpha = 0.5) +
    ggplot2::geom_point(
      shape = 21,
      alpha = 0.5,
      size = 3
    ) +
    ggplot2::facet_wrap(
      "parameter",
      ncol = 1,
      scales = "free"
      ) +
    ggplot2::scale_fill_viridis_c(direction = -1) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::xlab("") +
    ggplot2::ylab("RMSE")

  #message and plot
  if(verbose == TRUE){
    suppressMessages(print(p))
    cat("Best hyperparameters:  \n")
    cat(paste0("  - num.trees:     ", tuning[1, "num.trees"], "\n"))
    cat(paste0("  - mtry:          ", tuning[1, "mtry"], "\n"))
    cat(paste0("  - min.node.size: ", tuning[1, "min.node.size"], "\n"))
  }

  #preparing ranger arguments
  ranger.arguments$num.trees <- tuning[1, "num.trees"]
  ranger.arguments$mtry <- tuning[1, "mtry"]
  ranger.arguments$min.node.size <- tuning[1, "min.node.size"]

  out.list <- list()
  out.list$tuning <- tuning
  out.list$tuning.long <- tuning.long
  out.list$tuning.plot <- p
  out.list$ranger.arguments <- ranger.arguments

  out.list

}
