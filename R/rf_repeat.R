#' @title Fits several random forest models on the same data
#' @description Fits several random forest models on the same data in order to capture the effect of the algorithm's stochasticity on the variable importance scores, predictions, residuals, and performance measures. The function relies on the median to aggregate performance and importance values across repetitions. It is recommended to use it after a model is fitted ([rf()] or [rf_spatial()]), tuned ([rf_tuning()]), and/or evaluated ([rf_evaluate()]).
#' @param model A model fitted with [rf()]. If provided, the data and ranger arguments are taken directly from the model definition (stored in `model$ranger.arguments`). Default: `NULL`
#' @param data Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param dependent.variable.name Character string with the name of the response variable. Must be in the column names of `data`. If the dependent variable is binary with values 1 and 0, the argument `case.weights` of `ranger` is populated by the function [case_weights()]. Default: `NULL`
#' @param predictor.variable.names Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param distance.matrix Squared matrix with the distances among the records in `data`. The number of rows of `distance.matrix` and `data` must be the same. If not provided, the computation of the Moran's I of the residuals is omitted. Default: `NULL`
#' @param distance.thresholds Numeric vector with neighborhood distances. All distances in the distance matrix below each value in `dustance.thresholds` are set to 0 for the computation of Moran's I. If `NULL`, it defaults to seq(0, max(distance.matrix), length.out = 4). Default: `NULL`
#' @param xy (optional) Data frame or matrix with two columns containing coordinates and named "x" and "y". It is not used by this function, but it is stored in the slot `ranger.arguments$xy` of the model, so it can be used by [rf_evaluate()] and [rf_tuning()]. Default: `NULL`
#' @param ranger.arguments Named list with \link[ranger]{ranger} arguments (other arguments of this function can also go here). All \link[ranger]{ranger} arguments are set to their default values except for 'importance', that is set to 'permutation' rather than 'none'. Please, consult the help file of \link[ranger]{ranger} if you are not familiar with the arguments of this function.
#' @param scaled.importance Logical. If `TRUE`, and 'importance = "permutation', the function scales 'data' with \link[base]{scale} and fits a new model to compute scaled variable importance scores. Default: `FALSE`
#' @param repetitions Integer, number of random forest models to fit. Default: `10`
#' @param keep.models Logical, if `TRUE`, the fitted models are returned in the `models` slot. Set to `FALSE` if the accumulation of models is creating issues with the RAM memory available. Default: `TRUE`.
#' @param seed Integer, random seed to facilitate reproduciblity. If set to a given number, the results of the function are always the same.
#' @param verbose Logical, ff `TRUE`, messages and plots generated during the execution of the function are displayed, Default: `TRUE`
#' @param n.cores Integer, number of cores to use. Default = `parallel::detectCores() - 1`
#' @param cluster.ips Character vector with the IPs of the machines in a cluster. The machine with the first IP will be considered the main node of the cluster, and will generally be the machine on which the R code is being executed.
#' @param cluster.cores Numeric integer vector, number of cores to use on each machine.
#' @param cluster.user Character string, name of the user (should be the same throughout machines). Defaults to the current system user.
#' @param cluster.port Character, port used by the machines in the cluster to communicate. The firewall in all computers must allow traffic from and to such port. Default: `"11000"`
#' @return A ranger model with several new slots:
#' \itemize{
#'   \item `ranger.arguments`: Stores the values of the arguments used to fit the ranger model.
#'   \item `importance`: A list containing a data frame with the predictors ordered by their importance, a ggplot showing the importance values, and local importance scores.
#'   \item `performance`: out-of-bag performance scores: R squared, pseudo R squared, RMSE, and normalized RMSE (NRMSE).
#'   \item `pseudo.r.squared`: computed as the correlation between the observations and the predictions.
#'   \item `residuals`: residuals, normality test of the residuals computed with [residuals_test()], and spatial autocorrelation of the residuals computed with [moran_multithreshold()].
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
#'  out$importance$per.variable
#'
#'  #per repetition
#'  out$importance$per.repetition
#'
#'  #variable importance plot
#'  out$importance$per.repetition.plot
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
#'  rf.repeat$importance$per.repetition.plot
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
  xy = NULL,
  ranger.arguments = NULL,
  scaled.importance = FALSE,
  repetitions = 10,
  keep.models = TRUE,
  seed = NULL,
  verbose = TRUE,
  n.cores = parallel::detectCores() - 1,
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

    #saving tuning and evaluation slots for later
    if("tuning" %in% names(model)){
      tuning <- model$tuning
    }
    if("evaluation" %in% names(model)){
      evaluation <- model$evaluation
    }

  }

  if(is.null(ranger.arguments)){
    ranger.arguments <- list()
  }
  ranger.arguments$num.threads <- 1
  ranger.arguments$seed <- NULL
  ranger.arguments$scaled.importance <- scaled.importance

  if(keep.models == TRUE){
    ranger.arguments$write.forest <- TRUE
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

  #executing repetitions
  i <- NULL
  repeated.models <- foreach::foreach(
    i = 1:repetitions,
    .verbose = FALSE
  ) %dopar% {

    #model on raw data
    m.i <- spatialRF::rf(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      xy = xy,
      ranger.arguments = ranger.arguments,
      scaled.importance = scaled.importance,
      seed = ifelse(is.null(seed), i, seed + i),
      verbose = FALSE
    )

    #gathering results
    out <- list()
    out$predictions <- m.i$predictions
    out$importance$local <- m.i$variable.importance.local
    out$importance <- m.i$importance$per.variable
    out$importance.local <- m.i$importance$local
    out$prediction.error <- m.i$prediction.error
    out$r.squared <- m.i$performance$r.squared
    out$r.squared.oob <- m.i$performance$r.squared.oob
    out$pseudo.r.squared <- m.i$performance$pseudo.r.squared
    out$rmse.oob <- m.i$prediction.error
    out$rmse <- m.i$performance$rmse
    out$nrmse <- m.i$performance$nrmse
    out$auc <- m.i$performance$auc
    out$residuals <- m.i$residuals

    #saving model
    if(keep.models == TRUE){
      #removing extra weight from the model
      m.i$ranger.arguments$distance.matrix <- NULL
      m.i$ranger.arguments$xy <- NULL
      m.i$ranger.arguments$data <- NULL

      #saving it
      out$model <- m.i
    }

    return(out)

  }#end of parallelized loop

  #stopping cluster
  if(exists("temp.cluster")){
    parallel::stopCluster(cl = temp.cluster)
  }

  #fitting model if keep.models  == FALSE
  if(keep.models == FALSE){
    if(!is.null(model)){
      m <- model
    } else {
      m <- rf(
        data = data,
        dependent.variable.name = dependent.variable.name,
        predictor.variable.names = predictor.variable.names,
        distance.matrix = distance.matrix,
        distance.thresholds = distance.thresholds,
        xy = xy,
        ranger.arguments = ranger.arguments,
        scaled.importance = scaled.importance,
        seed = seed,
        verbose = FALSE
      )
    }
  } else {
    if(!is.null(model)){
      m <- model
    } else {
      m <- repeated.models[[1]]$model
    }
  }

  #PARSING OUTPUT OF PARALLELIZED LOOP
  ###################################

  #names of repetitions columns
  repetition.columns <- paste(
    "repetition",
    seq(1, repetitions),
    sep = "_"
    )


  #PREPARING predictions
  #------------------------------
  predictions.per.repetition <- as.data.frame(
    do.call(
      "cbind",
      lapply(
      lapply(
        repeated.models,
        "[[",
        "predictions"
      ),
      "[[",
      1
    )
    )
  )
  colnames(predictions.per.repetition) <- repetition.columns

  #computing medians
  predictions.median <- data.frame(
    median = apply(predictions.per.repetition, 1, FUN = median),
    median_absolute_deviation = apply(predictions.per.repetition, 1, stats::mad),
    row.names = NULL
  )

  m$predictions <- NULL
  m$predictions$values <- predictions.median$median
  m$predictions$values.per.repetition <- predictions.per.repetition
  m$predictions$values.median <- predictions.median



  #PREPARING variable.importance
  #-----------------------------
  m$importance <- NULL

  #per repetition
  importance.per.repetition <- as.data.frame(
    do.call(
      "rbind",
      lapply(
        repeated.models,
        "[[",
        "importance"
      )
    )
  )

  #median variable importance across repetitions
  importance.per.variable <- importance.per.repetition %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(importance = median(importance)) %>%
    dplyr::arrange(dplyr::desc(importance)) %>%
    as.data.frame()

  #saving importance info
  m$importance <- list()
  m$importance$per.variable <- importance.per.variable
  m$importance$per.variable.plot <- plot_importance(
    importance.per.variable,
    verbose = FALSE
  )
  m$importance$per.repetition <- importance.per.repetition
  m$importance$per.repetition.plot <- plot_importance(
    importance.per.repetition,
    verbose = verbose
  )

  #additional importance data if model is rf_spatial
  if(!is.null(model)){
    if(inherits(model, "rf_spatial")){

      #spatial predictors only
      spatial.predictors <- importance.per.repetition[grepl(
        "spatial_predictor",
        importance.per.repetition$variable
      ),]
      spatial.predictors$variable <- "spatial_predictors"

      #non-spatial predictors
      non.spatial.predictors <- importance.per.repetition[!grepl(
        "spatial_predictor",
        importance.per.repetition$variable
      ),]

      #spatial.predictors
      m$importance$spatial.predictors <- rbind(
        spatial.predictors,
        non.spatial.predictors
      )
      m$importance$spatial.predictors.plot <- plot_importance(
        m$importance$spatial.predictors,
        verbose = verbose
      )

      #spatial.predictors.stat
      #min, max, median and mean of the spatial predictors
      #aggregating spatial predictors
      #min, max, median and mean of the spatial predictors
      spatial.predictors.stats <- data.frame(
        variable = c(
          "spatial_predictors (max)",
          "spatial_predictors (min)",
          "spatial_predictors (median)",
          "spatial_predictors (quantile 0.25)",
          "spatial_predictors (quantile 0.75)"
        ),
        importance = c(
          max(spatial.predictors$importance),
          min(spatial.predictors$importance),
          median(spatial.predictors$importance),
          quantile(spatial.predictors$importanc, probs = 0.25),
          quantile(spatial.predictors$importanc, probs = 0.75)
        )
      )

      non.spatial.predictors.stats <- non.spatial.predictors %>%
        dplyr::group_by(variable) %>%
        dplyr::summarise(
          importance = median(importance)
        ) %>%
        as.data.frame()

      m$importance$spatial.predictors.stats <- rbind(
        spatial.predictors.stats,
        non.spatial.predictors.stats
      )

      m$importance$spatial.predictors.stats.plot <- plot_importance(
        m$importance$spatial.predictors.stats,
        verbose = verbose
      )

    }
  }

  #PREPARING importance.local
  #-----------------------------------
  m$importance$local <- m$variable.importance.local <- as.data.frame(
    apply(
      simplify2array(
        lapply(
          repeated.models,
          "[[",
          "importance.local"
        )
      ),
      1:2,
      median
    )
  )

  # m$importance$local$mad <- m$variable.importance.local <- as.data.frame(
  #   apply(
  #     simplify2array(
  #       lapply(
  #         repeated.models,
  #         "[[",
  #         "importance.local"
  #       )
  #     ),
  #     1:2,
  #     mad
  #   )
  # )


  #PREPARING prediction.error SLOT
  #-------------------------------
  m$prediction.error <- unlist(
    lapply(
      repeated.models,
      "[[",
      "prediction.error"
    )
  ) %>%
    median()


  #PREPARING THE PERFORMANCE SLOT
  #------------------------------
  m$performance <- list()

  m$performance$r.squared.oob <- unlist(
    lapply(
      repeated.models,
      "[[",
      "r.squared.oob"
    )
  )

  m$performance$r.squared <- unlist(
    lapply(
      repeated.models,
      "[[",
      "r.squared"
    )
  )

  #gathering pseudo R squared
  m$performance$pseudo.r.squared <- unlist(
    lapply(
      repeated.models,
      "[[",
      "pseudo.r.squared"
    )
  )

  #gathering rmse.oob
  m$performance$rmse.oob <- unlist(
    lapply(
      repeated.models,
      "[[",
      "prediction.error"
    )
  ) %>%
    sqrt()

  #gathering rmse
  m$performance$rmse <- unlist(
    lapply(
      repeated.models,
      "[[",
      "rmse"
    )
  )
  names(m$performance$rmse) <- NULL

  #gathering nrmse
  m$performance$nrmse <- unlist(
    lapply(
      repeated.models,
      "[[",
      "nrmse"
    )
  )
  names(m$performance$nrmse) <- NULL

  #gathering auc
  m$performance$auc <- unlist(
    lapply(
      repeated.models,
      "[[",
      "auc"
    )
  )
  names(m$performance$auc) <- NULL


  #PREPARING THE RESIDUALS SLOT
  #----------------------------
  m$residuals <- list()

  #gathering residuals
  residuals <- do.call(
    "cbind",
    lapply(
      lapply(
        repeated.models,
        "[[",
        "residuals"
      ),
      "[[",
      1
    )
  ) %>%
    as.data.frame()
  colnames(residuals) <- repetition.columns

  residuals.median <- data.frame(
    median = apply(residuals, 1, FUN = median),
    median_absolute_deviation = apply(residuals, 1, stats::mad),
    row.names = NULL
  )

  m$residuals$values <- residuals.median$median
  m$residuals$values.median <- residuals.median
  m$residuals$values.repetitions <- residuals
  m$residuals$stats <- summary(residuals.median$median)

  #gathering autocorrelation
  if(!is.null(distance.matrix)){

    #getting m$residuals$autocorrelation$per.distance
    moran.repetitions <- do.call(
      "rbind",
      lapply(
        lapply(
          lapply(
            repeated.models,
            "[[",
            "residuals"
          ),
          "[[",
          3
        ),
        "[[",
        1)
    ) %>%
      dplyr::arrange(distance.threshold)
    moran.repetitions$repetition <- rep(
      1:repetitions,
      length(unique(moran.repetitions$distance.threshold))
    )

    p.value <- NULL
    interpretation <- NULL
    moran.median <- moran.repetitions %>%
      dplyr::group_by(distance.threshold) %>%
      dplyr::summarise(
        moran.i = median(moran.i),
        p.value = median(p.value),
        interpretation = statistical_mode(interpretation)
      ) %>%
      as.data.frame()

    m$residuals$autocorrelation$per.distance <- moran.median
    m$residuals$autocorrelation$per.repetition <- moran.repetitions
    m$residuals$autocorrelation$plot <- plot_moran(
      moran.repetitions,
      verbose = verbose
    )

    m$residuals$autocorrelation$max.moran <- median(
        unlist(
          lapply(
            lapply(
              lapply(
                repeated.models,
                "[[",
                "residuals"
              ),
              "[[",
              3
            ),
            "[[",
            2)
        )
      )

    m$residuals$autocorrelation$max.moran.distance.threshold <- statistical_mode(
      unlist(
        lapply(
          lapply(
            lapply(
              repeated.models,
              "[[",
              "residuals"
            ),
            "[[",
            3
          ),
          "[[",
          3)
      )
    )

  }

  #normality of the median residuals
  m$residuals$normality <- residuals_diagnostics(
    residuals = m$residuals$values,
    predictions = m$predictions$values
  )

  #plot of the residuals diagnostics
  m$residuals$diagnostics <- plot_residuals_diagnostics(
    m,
    verbose = verbose
  )

  #gathering models
  if(keep.models == TRUE){

    m$models <- lapply(
      repeated.models,
      "[[",
      "model"
    )

  }

  #adding repetitions to ranger.arguments
  m$ranger.arguments$repetitions <- repetitions
  m$ranger.arguments$keep.models <- keep.models

  #adding evaluation and tuning slots if they exist
  if(!is.null(model)){
    #saving tuning and evaluation slots for later
    if("tuning" %in% names(model)){
      m$tuning <- tuning
    }
    if("evaluation" %in% names(model)){
      m$evaluation <- evaluation
    }
    class(m) <- c(class(m), "rf_repeat")
  } else {
    class(m) <- c("rf", "rf_repeat", "ranger")
  }

  #print model
  if(verbose == TRUE){
    print(m)
  }

  #return m.curves
  m

}
