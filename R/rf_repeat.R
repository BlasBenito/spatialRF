#' @title Fits several random forest models on the same data
#' @description Fits several random forest models on the same data in order to capture the effect of the algorithm's stochasticity on the importance scores and performance measures.
#' @param model A model fitted with [rf()]. If provided, the data and ranger arguments are taken directly from the model definition (stored in `model$ranger.arguments`). Default: `NULL`
#' @param data Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param dependent.variable.name Character string with the name of the response variable. Must be in the column names of `data`. If the dependent variable is binary with values 1 and 0, the argument `case.weights` of `ranger` is populated by the function [case_weights()]. Default: `NULL`
#' @param predictor.variable.names Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param distance.matrix Squared matrix with the distances among the records in `data`. The number of rows of `distance.matrix` and `data` must be the same. If not provided, the computation of the Moran's I of the residuals is omitted. Default: `NULL`
#' @param distance.thresholds Numeric vector with neighborhood distances. All distances in the distance matrix below each value in `dustance.thresholds` are set to 0 for the computation of Moran's I. If `NULL`, it defaults to seq(0, max(distance.matrix), length.out = 4). Default: `NULL`
#' @param ranger.arguments Named list with \link[ranger]{ranger} arguments (other arguments of this function can also go here). All \link[ranger]{ranger} arguments are set to their default values except for 'importance', that is set to 'permutation' rather than 'none'. Please, consult the help file of \link[ranger]{ranger} if you are not familiar with the arguments of this function.
#' @param scaled.importance Logical. If `TRUE`, and 'importance = "permutation', the function scales 'data' with \link[base]{scale} and fits a new model to compute scaled variable importance scores. Default: `TRUE`
#' @param repetitions Integer, number of random forest models to fit. Default: `5`
#' @param keep.models Logical, if `TRUE`, the fitted models are returned in the `models` slot. Set to `FALSE` if the accumulation of models is creating issues with the RAM memory available. Default: `TRUE`.
#' @param seed Integer, random seed to facilitate reproduciblity. If set to a given number, the results of the function are always the same.
#' @param verbose Logical, ff `TRUE`, messages and plots generated during the execution of the function are displayed, Default: `TRUE`
#' @param n.cores Integer, number of cores to use during computations. If `NULL`, all cores but one are used, unless a cluster is used. Default = `NULL`
#' @param cluster.ips Character vector with the IPs of the machines in a cluster. The machine with the first IP will be considered the main node of the cluster, and will generally be the machine on which the R code is being executed.
#' @param cluster.cores Numeric integer vector, number of cores to use on each machine.
#' @param cluster.user Character string, name of the user (should be the same throughout machines). Defaults to the current system user.
#' @param cluster.port Character, port used by the machines in the cluster to communicate. The firewall in all computers must allow traffic from and to such port. Default: `"11000"`
#' @return A ranger model with several new slots:
#' \itemize{
#'   \item `ranger.arguments`: Stores the values of the arguments used to fit the ranger model.
#'   \item `variable.importance`: A list containing a data frame with the predictors ordered by their importance, and a ggplot showing the importance values.
#'   \item `performance`: out-of-bag performance scores: R squared, pseudo R squared, RMSE, and normalized RMSE (NRMSE).
#'   \item `pseudo.r.squared`: computed as the correlation between the observations and the predictions.
#'   \item `residuals`: computed as observations minus predictions.
#'   \item `spatial.correlation.residuals`: the result of [moran_multithreshold()] applied to the model results.
#' }
#' @examples
#' \donttest{
#' if(interactive()){
#'
#'  data(plant_richness_df)
#'  data(distance_matrix)
#'
#'  out <- rf_repeat(
#'    data = plant_richness_df,
#'    dependent.variable.name = "richness_species_vascular",
#'    predictor.variable.names = colnames(plant_richness_df)[5:21],
#'    distance.matrix = distance_matrix,
#'    distance.thresholds = c(0, 100, 1000, 10000),
#'    repetitions = 5,
#'    n.cores = 1
#'  )
#'
#'  class(out)
#'
#'  #data frame with ordered variable importance
#'  out$variable.importance$per.variable
#'
#'  #per repetition
#'  out$variable.importance$per.repetition
#'
#'  #variable importance plot
#'  out$variable.importance$per.repetition.plot
#'
#'  #performance
#'  out$performance
#'
#'
#'  #spatial correlation of the residuals for different distance thresholds
#'  out$spatial.correlation.residuals$per.distance
#'
#'  #plot of the Moran's I of the residuals for different distance thresholds
#'  out$spatial.correlation.residuals$plot
#'
#'  #using a model as an input for rf_repeat()
#'  rf.model <- rf(
#'    data = plant_richness_df,
#'    dependent.variable.name = "richness_species_vascular",
#'    predictor.variable.names = colnames(plant_richness_df)[8:21],
#'    distance.matrix = distance_matrix,
#'    distance.thresholds = c(0, 1000, 2000)
#'    )
#'
#'  rf.repeat <- rf_repeat(model = rf.model)
#'  rf.repeat$performance
#'  rf.repeat$variable.importance$per.repetition.plot
#'
#' }
#' }
#' @importFrom tidyselect all_of
#' @rdname rf_repeat
#' @export
rf_repeat <- function(
  model = NULL,
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  ranger.arguments = NULL,
  scaled.importance = TRUE,
  repetitions = 5,
  keep.models = TRUE,
  seed = NULL,
  verbose = TRUE,
  n.cores = NULL,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = Sys.info()[["user"]],
  cluster.port = "11000"
){

  #declaring some variables
  variable <- NULL
  importance <- NULL
  distance.threshold <- NULL
  moran.i <- NULL

  #checking repetitions
  if(!is.integer(repetitions)){
    repetitions <- floor(repetitions)
  }
  if(repetitions < 5){
    repetitions <- 5
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
    importance <- ranger.arguments$importance
    local.importance <- ranger.arguments$local.importance
  }

  if(is.null(ranger.arguments)){
    ranger.arguments <- list()
  }
  ranger.arguments$importance <- importance <- "permutation"
  ranger.arguments$local.importance <- local.importance <- FALSE
  ranger.arguments$num.threads <- 1
  ranger.arguments$seed <- NULL
  ranger.arguments$scaled.importance <- scaled.importance

  if(keep.models == TRUE){
    ranger.arguments$write.forest <- TRUE
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
  if(is.null(cluster.ips)){

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

  #executing repetitions
  i <- NULL
  repeated.models <- foreach::foreach(
    i = 1:repetitions
  ) %dopar% {

    #model on raw data
    m.i <- spatialRF::rf(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments,
      scaled.importance = scaled.importance,
      seed = ifelse(is.null(seed), i, seed + i),
      verbose = FALSE
    )

    #gathering results
    out <- list()
    out$predictions <- m.i$predictions
    if(!is.null(local.importance)){
      if(local.importance == TRUE){
        out$variable.importance.local <- m.i$variable.importance.local
      }
    }
    out$variable.importance <- m.i$variable.importance$per.variable
    out$prediction.error <- m.i$prediction.error
    out$r.squared <- m.i$performance$r.squared
    out$pseudo.r.squared <- m.i$performance$pseudo.r.squared
    out$rmse <- m.i$performance$rmse
    out$nrmse <- m.i$performance$nrmse
    out$auc <- m.i$performance$auc
    out$residuals <- m.i$residuals
    out$spatial.correlation.residuals <- m.i$spatial.correlation.residuals

    #saving model
    if(keep.models == TRUE){
      out$model <- m.i
    }

    return(out)

  }#end of parallelized loop

  #fitting model to allow plotting partial dependence curves
  m.curves <- rf(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    distance.matrix = distance.matrix,
    distance.thresholds = distance.thresholds,
    ranger.arguments = ranger.arguments,
    scaled.importance = scaled.importance,
    seed = seed,
    verbose = FALSE
  )

  #PARSING OUTPUT OF PARALLELIZED LOOP

  #names of repetitions columns
  repetition.columns <- paste("repetition", 1:repetitions, sep = "_")

  #gathering predictions
  predictions.per.repetition <- as.data.frame(
    do.call(
      "cbind",
      lapply(
        repeated.models,
        "[[",
        "predictions"
      )
    )
  )
  colnames(predictions.per.repetition) <- repetition.columns
  predictions.mean <- data.frame(
    mean = rowMeans(predictions.per.repetition),
    standard_deviation = apply(predictions.per.repetition, 1, sd)
  )
  m.curves$predictions <- NULL
  m.curves$predictions$per.repetition <- predictions.per.repetition
  m.curves$predictions$mean <- predictions.mean

  #gathering variable.importance.local
  if(!is.null(local.importance)){
    if(local.importance == TRUE){
      m.curves$variable.importance.local <- as.data.frame(
        apply(
          simplify2array(
            lapply(
              repeated.models,
              "[[",
              "variable.importance.local"
            )
          ),
          1:2,
          mean
        )
      )
    }
  }


  if(importance == "permutation"){

    #gathering variable.importance
    m.curves$variable.importance <- NULL

    #per repetition
    variable.importance.per.repetition <- as.data.frame(
      do.call(
        "rbind",
        lapply(
          repeated.models,
          "[[",
          "variable.importance"
        )
      )
    )

    #mean
    variable.importance.per.variable <- variable.importance.per.repetition %>%
      dplyr::group_by(variable) %>%
      dplyr::summarise(importance = mean(importance)) %>%
      dplyr::arrange(dplyr::desc(importance)) %>%
      as.data.frame()

    m.curves$variable.importance <- list()
    m.curves$variable.importance$per.variable <- variable.importance.per.variable
    m.curves$variable.importance$per.variable.plot <- plot_importance(
      variable.importance.per.variable,
      verbose = FALSE
      )
    m.curves$variable.importance$per.repetition <- variable.importance.per.repetition
    m.curves$variable.importance$per.repetition.plot <- plot_importance(
      x = variable.importance.per.repetition,
      verbose = verbose
    )

  }

  #gathering prediction.error
  m.curves$prediction.error <- unlist(
    lapply(
      repeated.models,
      "[[",
      "prediction.error"
    )
  )

  #gathering r.squared
  m.curves$performance <- list()
  m.curves$performance$r.squared <- unlist(
    lapply(
      repeated.models,
      "[[",
      "r.squared"
    )
  )
  m.curves$performance$r.squared <- round(m.curves$performance$r.squared, 3)

  #gathering pseudo R squared
  m.curves$performance$pseudo.r.squared <- unlist(
    lapply(
      repeated.models,
      "[[",
      "pseudo.r.squared"
    )
  )

  #gathering rmse
  m.curves$performance$rmse <- unlist(
    lapply(
      repeated.models,
      "[[",
      "rmse"
    )
  )
  names(m.curves$performance$rmse) <- NULL

  #gathering nrmse
  m.curves$performance$nrmse <- unlist(
    lapply(
      repeated.models,
      "[[",
      "nrmse"
    )
  )
  names(m.curves$performance$nrmse) <- NULL

  #gathering auc
  m.curves$performance$auc <- unlist(
    lapply(
      repeated.models,
      "[[",
      "auc"
    )
  )
  names(m.curves$performance$auc) <- NULL

  #gathering spatial.correlation.residuals
  if(!is.null(distance.matrix)){

    spatial.correlation.residuals.per.repetition <- do.call(
      "rbind",
      lapply(
        lapply(
          repeated.models,
          "[[",
          "spatial.correlation.residuals"
        ),
        "[[",
        1
      )
    ) %>%
      dplyr::arrange(distance.threshold)
    spatial.correlation.residuals.per.repetition$repetition <- rep(
      1:repetitions,
      length(unique(spatial.correlation.residuals.per.repetition$distance.threshold))
      )

    p.value <- NULL
    interpretation <- NULL
    spatial.correlation.residuals.mean <- spatial.correlation.residuals.per.repetition %>%
      dplyr::group_by(distance.threshold) %>%
      dplyr::summarise(
        moran.i = mean(moran.i),
        p.value = mean(p.value),
        interpretation = statistical_mode(interpretation)
      ) %>%
      as.data.frame()

    m.curves$spatial.correlation.residuals <- list()
    m.curves$spatial.correlation.residuals$per.distance <- spatial.correlation.residuals.mean
    m.curves$spatial.correlation.residuals$per.repetition <- spatial.correlation.residuals.per.repetition
    m.curves$spatial.correlation.residuals$plot <- plot_moran(
      x = spatial.correlation.residuals.per.repetition,
      verbose = verbose
    )

    m.curves$spatial.correlation.residuals$max.moran <-  mean(
      unlist(
        lapply(
          lapply(
            repeated.models,
            "[[",
            "spatial.correlation.residuals"
          ),
          "[[",
          2
        )
      )
    )

    m.curves$spatial.correlation.residuals$max.moran.distance.threshold <- statistical_mode(
      unlist(
        lapply(
          lapply(
            repeated.models,
            "[[",
            "spatial.correlation.residuals"
          ),
          "[[",
          3
        )
      )
    )

  }

  #gathering residuals
  residuals <- as.data.frame(do.call("cbind", lapply(
    repeated.models,
    "[[",
    "residuals"
  )))
  colnames(residuals) <- repetition.columns

  residuals.mean <- data.frame(
    mean = rowMeans(residuals),
    standard_deviation = apply(residuals, 1, sd),
    row.names = NULL
  )

  m.curves$residuals <- NULL
  m.curves$residuals$mean <- residuals.mean
  m.curves$residuals$per.repetition <- residuals
  m.curves$residuals$stats <- summary(residuals.mean$mean)

  #gathering models
  if(keep.models == TRUE){

    m.curves$models <-    lapply(
      repeated.models,
      "[[",
      "model"
    )

  }

  #adding repetitions to ranger.arguments
  m.curves$ranger.arguments$repetitions <- repetitions
  m.curves$ranger.arguments$keep.models <- keep.models

  #adding class to the model
  class(m.curves) <- c("rf", "rf_repeat", "ranger")

  #print model
  if(verbose == TRUE){
    print(m.curves)
  }

  #return m.curves
  m.curves


}
