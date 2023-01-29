#' @title Fits several random forest models on the same data
#' @description Fits several random forest models on the same data in order to capture the effect of the algorithm's stochasticity on the variable importance scores, predictions, residuals, and performance measures. The function relies on the median to aggregate performance and importance values across repetitions. It is recommended to use it after a model is fitted ([rf()] or [rf_spatial()]), tuned ([rf_tuning()]), and/or evaluated ([rf_evaluate()]). This function is designed to be used after fitting a model with [rf()] or [rf_spatial()], tuning it with [rf_tuning()] and evaluating it with [rf_evaluate()].
#' @param model (required if `data` is `NULL`; model produced with `spatialRF`) A model fitted with [rf()]. If provided, the data and ranger arguments are taken directly from the model definition (stored in `model$ranger_arguments`), and the argument `ranger.arguments` is ignored. Default: `NULL`
#' @param data (required if `model` is `NULL`; data frame or tibble) Data frame with a response variable and a set of predictors. If `data` is a tibble, all data frames in the output model are coerced to tibble. Default: `NULL`
#' @param response.name (required if `model` is `NULL`; character string) Character string with the name of the response variable. Must be in the column names of `data`. If the dependent variable is binary with values 1 and 0, the argument `case.weights` of `ranger` is populated by the function [case_weights()]. Default: `NULL`
#' @param predictors.names (required if `model` is `NULL`; character vector with column names of `data`) Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param distance.matrix (optional; distance matrix) Squared matrix with the distances among the records in `data`. The number of rows of `distance.matrix` and `data` must be the same. If not provided, the computation of the Moran's I of the residuals is omitted. Default: `NULL`
#' @param distance.thresholds (optional; numeric vector with distances in the same units as `distance.matrix`) Numeric vector with neighborhood distances. All distances in the distance matrix below each value in `dustance.thresholds` are set to 0 for the computation of Moran's I. If `NULL`, it defaults to seq(0, max(distance.matrix), length.out = 4). Default: `NULL`
#' @param xy (optional; data frame, tibble, or matrix) Data frame or matrix with two columns containing coordinates and named "x" and "y". It is not used by this function, but it is stored in the slot `ranger.arguments$xy` of the model, so it can be used by [rf_evaluate()] and [rf_tuning()]. Default: `NULL`
#' @param ranger.arguments (optional; list with ranger::ranger() arguments) Named list with \link[ranger]{ranger} arguments (other arguments of this function can also go here). All \link[ranger]{ranger} arguments are set to their default values except for 'importance', that is set to 'permutation' rather than 'none'. Please, consult the help file of \link[ranger]{ranger} if you are not familiar with the arguments of this function.
#' @param scaled.importance (optional; logical) If `TRUE`, and 'importance = "permutation', the function scales 'data' with \link[base]{scale} and fits a new model to compute scaled variable importance scores. Default: `FALSE`
#' @param repetitions Integer, number of random forest models to fit. Default: `10`
#' @param keep.models Logical, if `TRUE`, the fitted models are returned in the `models` slot. Set to `FALSE` if the accumulation of models is creating issues with the RAM memory available. Default: `TRUE`.
#' @param seed Integer, random seed to facilitate reproduciblity. If set to a given number, the results of the function are always the same. Default: `1`.
#' @param verbose (optional; logical) If TRUE, messages and plots generated during the execution of the function are displayed. Default: `TRUE`
#' @param n.cores  (optional; integer) Number of cores to use. Default: `parallel::detectCores() - 1`
#' @param cluster (optional; cluster object) A cluster definition generated with `parallel::makeCluster()` or \code{\link{start_cluster}}. Only advisable if you need to spread a large number of repetitions over the nodes of a large cluster when working with large data. If provided, overrides `n.cores`. The function does not stop a cluster, please remember to shut it down with `parallel::stopCluster(cl = cluster_name)` or `spatialRF::stop_cluster()` at the end of your pipeline. Default: `NULL`
#' @return A ranger model with several new slots:
#' \itemize{
#'   \item `ranger.arguments`: Stores the values of the arguments used to fit the ranger model.
#'   \item `importance`: A list containing a data frame with the predictors ordered by their importance, a ggplot showing the importance values, and local importance scores.
#'   \item `predictions`: Predicted values.
#'   \itemize{
#'    \item `ib`: Numeric vector, median across repetitions of the predictions computed on the in-bag data.
#'    \item `oob`: Numeric vector, median across repetitions of the predictions computed on the out-of-bag data.
#'    \item `ib.repetitions`: Data frame with one column per repetition containing predictions computed on the in-bag data.
#'    \item `oob.repetitions`: Data frame with one column per repetition containing predictions computed on the out-of-bag data.
#'   }
#'   \item `performance`: Performance scores computed on the in-bag and out-of-bag data. These performance scores can be highly inflated (especially the "in-bag" ones!)  if the spatial structure of the training data is strong, so I beg you to never report these as the actual performance metrics of your models, and advise you to use [rf_evaluate()] instead. In any case, if you wish to proceed, these are the metrics provided in this slot:
#'   \itemize{
#'     \item `rsquared_oob`: R-squared computed on the out-of-bag predictions using the expression `cor(observed, predicted_oob) ^ 2`.
#'     \item `rmse_oob`: Root mean squared error computed on the out-of-bag predictions using the expression `spatialRF::root_mean_squared_error(o = observed, p = predicted_oob)`.
#'     \item `nrmse_oob`: Normalized rood mean squared error computed on the out-of-bag data using the expression `spatialRF::root_mean_squared_error(o = observed, p = predicted_oob, normalization = "iq")`.
#'     \item `auc_oob`: Only for binary responses with values 0 and 1. Area under the ROC curve computed on the out-of-bag predictions using the expression `spatialRF::auc(o = observed, p = predicted_oob)`.
#'     \item `rsquared_full`: R-squared computed on the in-bag predictions using the expression `cor(observed, predicted_full) ^ 2`.
#'     \item `rmse_full`: Root mean squared error computed on the in-bag predictions using the expression `spatialRF::root_mean_squared_error(o = observed, p = predicted_full)`.
#'     \item `nrmse_full`: Normalized rood mean squared error computed on the in-bag data using the expression `spatialRF::root_mean_squared_error(o = observed, p = predicted_full, normalization = "iq")`.
#'     \item `auc_full`: Only for binary responses with values 0 and 1. Area under the ROC curve computed on the in-bag predictions using the expression `spatialRF::auc(o = observed, p = predicted_full)`.
#'   }
#'   \item `residuals`: residuals, normality test of the residuals computed with [residuals_test()], and spatial autocorrelation of the residuals computed with [moran_multithreshold()].
#' }
#'
#' @examples
#' if(interactive()){
#'
#'  #loading example data
#' data(
#'   ecoregions_df,
#'   ecoregions_distance_matrix,
#'   ecoregions_numeric_predictors,
#'   ecoregions_continuous_response
#'   )
#'
#'  #fitting 10 random forest models
#'  out <- rf_repeat(
#'    data = ecoregions_df,
#'    response.name = ecoregions_continuous_response,
#'    predictors.names = ecoregions_numeric_predictors,
#'    distance.matrix = ecoregions_distance_matrix,
#'    distance.thresholds = 0,
#'    repetitions = 10,
#'    n.cores = 1
#'  )
#'
#'  #data frame with median variable importance
#'  out$importance$global
#'
#'  #importance per repetition
#'  out$importance$per_repetition
#'
#'  #variable importance plot
#'  out$importance$per_repetition.plot
#'
#'  #also
#'  plot_importance(out)
#'
#'  #performance per repetition
#'  out$performance
#'
#'
#'  #spatial correlation of the residuals for different distance thresholds
#'  out$spatial.correlation.residuals$per_distance
#'
#'  #plot of the Moran's I of the residuals for different distance thresholds
#'  out$spatial.correlation.residuals$plot
#'
#'  #using a model as an input for rf_repeat()
#'  rf.model <- rf(
#'    data = ecoregions_df,
#'    response.name = ecoregions_continuous_response,
#'    predictors.names = ecoregions_numeric_predictors,
#'    distance.matrix = ecoregions_distance_matrix,
#'    distance.thresholds = 0,
#'    n.cores = 1
#'    )
#'
#'  #repeating the model 5 times
#'  rf.repeat <- rf_repeat(
#'    model = rf.model,
#'    repetitions = 5,
#'    n.cores = 1
#'    )
#'
#'
#'  rf.repeat$performance
#'  rf.repeat$importance$per_repetition.plot
#'
#' }
#' @importFrom tidyselect all_of
#' @rdname rf_repeat
#' @export
rf_repeat <- function(
  model = NULL,
  data = NULL,
  response.name = NULL,
  predictors.names = NULL,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  xy = NULL,
  ranger.arguments = NULL,
  scaled.importance = FALSE,
  repetitions = 10,
  keep.models = TRUE,
  seed = 1,
  verbose = TRUE,
  n.cores = parallel::detectCores() - 1,
  cluster = NULL
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

  #HANDLING ARGUMENTS
  ######################################

  #if model is provided
  if(!is.null(model)){

    #stopping if model is not of the right class
    if(!("rf" %in% class(model))){
      stop("The argument 'model' is not of the class 'rf'.")
    }

    #RULE 1: if ranger.arguments is not provided
    if(is.null(ranger.arguments)){

      #overriding input arguments
      data <- NULL
      response.name <- NULL
      predictors.names <- NULL
      distance.matrix <- NULL
      distance.thresholds <- NULL
      xy <- NULL

      #writing the model's ranger.arguments to the environment
      ranger.arguments <- model$ranger_arguments

      #writing arguments to the function environment
      list2env(model$ranger_arguments, envir=environment())

    } else {

      #RULE 2:
      #input arguments in model$ranger_arguments take precedence

      ranger.arguments$data <- NULL
      ranger.arguments$response.name <- NULL
      ranger.arguments$predictors.names <- NULL
      ranger.arguments$distance.matrix <- NULL
      ranger.arguments$distance.thresholds <- NULL
      ranger.arguments$xy <- NULL

      #writing arguments to the function environment
      list2env(model$ranger_arguments, envir=environment())
      list2env(ranger.arguments, envir=environment())


    }

  } else {
    #RULE 3:

    if(!is.null(ranger.arguments)){

      if(is.null(data)){
        data <- model$ranger_arguments$data
      }

      if(is.null(response.name)){
        response.name <- model$ranger_arguments$response.name
      }

      if(is.null(predictors.names)){
        predictors.names <- model$ranger_arguments$predictors.names
      }

      if(is.null(distance.matrix)){
        distance.matrix <- model$ranger_arguments$distance.matrix
      }

      if(is.null(distance.thresholds)){
        distance.thresholds <- model$ranger_arguments$distance.thresholds
      }

      if(is.null(xy)){
        xy <- model$ranger_arguments$xy
      }

      if(is.null(cluster)){
        cluster <- model$ranger_arguments$cluster
      }

      if(is.null(seed)){
        seed <- model$ranger_arguments$seed
      }

      #writing ranger.arguments to the function environment
      list2env(ranger.arguments, envir=environment())

    }

  }

  #predictors.names comes from mc_auto_vif or mc_auto_cor
  if(inherits(predictors.names, "variable_selection")){

    predictors.names <- predictors.names$selected.variables

  }

  #check if input is tibble
  if(tibble::is_tibble(data) == TRUE){
    return.tibble <- TRUE
  } else {
    return.tibble <- FALSE
  }

  #END OF HANDLING ARGUMENTS
  ##########################

  # # ranger arguments for
  # if(is.null(ranger.arguments)){
  #   ranger.arguments <- list(
  #     # num.threads = 1,
  #     seed = NULL,
  #     scaled.importance = scaled.importance
  #   )
  # }

  if(keep.models == TRUE){
    ranger.arguments$write.forest <- TRUE
  }


  #HANDLING PARALLELIZATION
  ##########################
  if("cluster" %in% class(cluster)){

    #registering cluster
    doParallel::registerDoParallel(cl = cluster)

    #parallel iterator
    `%iterator%` <- foreach::`%dopar%`

    #in-loop cores and ranger arguments
    in.loop.n.cores <- 1
    in.loop.ranger.arguments <- ranger.arguments
    in.loop.ranger.arguments$num.threads <- 1


  } else {

    #sequential iterator
    `%iterator%` <- foreach::`%do%`

    #in-loop cores and ranger arguments
    in.loop.n.cores <- n.cores
    in.loop.ranger.arguments <- ranger.arguments

  }

  #removing ranger arguments from in.loop.ranger.arguments
  in.loop.ranger.arguments$seed <- NULL

  ##########################
  #END OF HANDLING PARALLELIZATION

  #loop
  i <- NULL
  repeated.models <- foreach::foreach(
    i = 1:repetitions,
    .verbose = FALSE
  ) %iterator% {

    #model on raw data
    m.i <- spatialRF::rf(
      data = data,
      response.name = response.name,
      predictors.names = predictors.names,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      xy = xy,
      ranger.arguments = in.loop.ranger.arguments,
      scaled.importance = scaled.importance,
      seed = seed + i,
      n.cores = in.loop.n.cores,
      verbose = FALSE
    )

    #gathering results
    out <- list()
    out$predictions <- m.i$predictions
    out$importance$local <- m.i$variable_importance_local
    out$importance <- m.i$importance$global
    out$importance.local <- as.matrix(m.i$importance$local)
    out$prediction.error <- m.i$prediction.error
    out$rsquared_full <- m.i$performance$rsquared_full
    out$rsquared_oob <- m.i$performance$rsquared_oob
    out$rmse_full <- m.i$performance$rmse_full
    out$rmse_oob <- m.i$performance$rmse_oob
    out$nrmse_full <- m.i$performance$nrmse_full
    out$nrmse_oob <- m.i$performance$nrmse_oob
    out$auc_full <- m.i$performance$auc_full
    out$auc_oob <- m.i$performance$auc_oob
    out$residuals <- m.i$residuals

    #saving model
    if(keep.models == TRUE){

      #removing extra weight from the model
      m.i$ranger_arguments$distance.matrix <- NULL
      m.i$ranger_arguments$xy <- NULL
      m.i$ranger_arguments$data <- NULL

      #saving it
      out$model <- m.i
    }

    return(out)

  }#end of parallelized loop

  #fitting model if keep.models  == FALSE
  if(keep.models == FALSE){

    if(!is.null(model)){

      m <- model

    } else {

      m <- rf(
        data = data,
        response.name = response.name,
        predictors.names = predictors.names,
        distance.matrix = distance.matrix,
        distance.thresholds = distance.thresholds,
        xy = xy,
        ranger.arguments = in.loop.ranger.arguments,
        scaled.importance = scaled.importance,
        seed = seed,
        n.cores = in.loop.n.cores,
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
  predictions.per.repetition_full <- as.data.frame(
    do.call(
      "cbind",
      lapply(
      lapply(
        repeated.models,
        "[[",
        "predictions"
      ),
      "[[",
      "ib"
    )
    )
  )
  colnames(predictions.per.repetition_full) <- repetition.columns

  predictions.per.repetition_oob <- as.data.frame(
    do.call(
      "cbind",
      lapply(
        lapply(
          repeated.models,
          "[[",
          "predictions"
        ),
        "[[",
        "oob"
      )
    )
  )
  colnames(predictions.per.repetition_oob) <- repetition.columns

  #computing medians
  # predictions_full.median <- data.frame(
  #   median = apply(
  #     predictions.per.repetition_full,
  #     1,
  #     FUN = median
  #     ),
  #   median_absolute_deviation = apply(
  #     predictions.per.repetition_full,
  #     1, stats::mad
  #     ),
  #   row.names = NULL
  # )
  #
  # predictions_oob.median <- data.frame(
  #   median = apply(
  #     predictions.per.repetition_oob,
  #     1,
  #     FUN = median
  #   ),
  #   median_absolute_deviation = apply(
  #     predictions.per.repetition_oob,
  #     1, stats::mad
  #   ),
  #   row.names = NULL
  # )

  m$predictions <- NULL
  m$predictions$full <- apply(
    predictions.per.repetition_full,
    1,
    FUN = median
  )
  m$predictions$oob <- apply(
    predictions.per.repetition_oob,
    1,
    FUN = median
  )
  m$predictions$repetitions_full <- predictions.per.repetition_full
  m$predictions$repetitions_oob <- predictions.per.repetition_oob



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
  m$importance$global <- importance.per.variable
  m$importance$plot <- plot_importance(
    model = importance.per.variable,
    verbose = FALSE
  )

  if(repetitions > 1){
    m$importance$per_repetition <- importance.per.repetition
  }

  #violin plot only if repetitions equal or larger than 10
  if(repetitions >= 10){
    m$importance$per_repetition.plot <- plot_importance(
      model = importance.per.repetition,
      verbose = verbose
    )
  }



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
      m$importance$spatial_predictors <- rbind(
        spatial.predictors,
        non.spatial.predictors
      )
      m$importance$spatial_predictors.plot <- plot_importance(
        m$importance$spatial_predictors,
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
          quantile(spatial.predictors$importance, probs = 0.25),
          quantile(spatial.predictors$importance, probs = 0.75)
        )
      )

      non.spatial.predictors.stats <- non.spatial.predictors %>%
        dplyr::group_by(variable) %>%
        dplyr::summarise(
          importance = median(importance)
        ) %>%
        as.data.frame()

      m$importance$spatial_predictors.stats <- rbind(
        spatial.predictors.stats,
        non.spatial.predictors.stats
      )

      m$importance$spatial_predictors_stats_plot <- plot_importance(
        m$importance$spatial_predictors.stats,
        verbose = verbose
      )

    }
  }

  #PREPARING importance.local
  #-----------------------------------
  m$importance$local <- m$variable_importance_local <- as.data.frame(
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

  # m$importance$local$mad <- m$variable_importance_local <- as.data.frame(
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

  m$performance$rsquared_oob <- unlist(
    lapply(
      repeated.models,
      "[[",
      "rsquared_oob"
    )
  )

  m$performance$rmse_oob <- unlist(
    lapply(
      repeated.models,
      "[[",
      "rmse_oob"
    )
  )
  names(m$performance$rmse_oob) <- NULL

  m$performance$nrmse_oob <- unlist(
    lapply(
      repeated.models,
      "[[",
      "nrmse_oob"
    )
  )
  names(m$performance$nrmse_oob) <- NULL

  m$performance$auc_oob <- unlist(
    lapply(
      repeated.models,
      "[[",
      "auc_oob"
    )
  )
  names(m$performance$auc_oob) <- NULL

  m$performance$rsquared_full <- unlist(
    lapply(
      repeated.models,
      "[[",
      "rsquared_full"
    )
  )

  m$performance$rmse_full <- unlist(
    lapply(
      repeated.models,
      "[[",
      "rmse_full"
    )
  )
  names(m$performance$rmse_full) <- NULL

  m$performance$nrmse_full <- unlist(
    lapply(
      repeated.models,
      "[[",
      "nrmse_full"
    )
  )
  names(m$performance$nrmse_full) <- NULL

  m$performance$auc_full <- unlist(
    lapply(
      repeated.models,
      "[[",
      "auc_full"
    )
  )
  names(m$performance$auc_full) <- NULL



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
  m$residuals$values_median <- residuals.median
  m$residuals$values_repetitions <- residuals
  m$residuals$stats <- summary(residuals.median$median)

  #gathering autocorrelation
  if(!is.null(distance.matrix)){

    #getting m$residuals$autocorrelation$per_distance
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

    m$residuals$autocorrelation$per_distance <- moran.median
    m$residuals$autocorrelation$per_repetition <- moran.repetitions
    m$residuals$autocorrelation$plot <- plot_moran(
      moran.repetitions,
      verbose = verbose
    )

    m$residuals$autocorrelation$max_moran <- median(
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

    m$residuals$autocorrelation$max_moran_distance_threshold <- statistical_mode(
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
    predictions = m$predictions$full
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
  m$ranger_arguments$repetitions <- repetitions
  m$ranger_arguments$keep.models <- keep.models

  #adding evaluation and tuning slots if they exist
  if(!is.null(model)){

    #saving tuning and evaluation slots for later
    if("tuning" %in% names(model)){
      m$tuning <- model$tuning
    }

    if("evaluation" %in% names(model)){
      m$evaluation <- model$evaluation
    }

    if("variable.selection" %in% names(model)){
      m$variable_selection <- model$variable_selection
    }
  }

  #adding class to the model
  class(m) <- unique(c(class(m), "rf_repeat", "ranger"))

  #adding cluster to model
  if(!is.null(cluster) & "cluster" %in% class(cluster)){
    m$ranger_arguments$cluster <- cluster
  }

  #coercing output to tibble
  if(return.tibble == TRUE){
    m$variable_importance_local <- tibble::as_tibble(m$variable_importance_local)
    m$residuals$values_median <- tibble::as_tibble(m$residuals$values_median)
    m$residuals$values_repetitions <- tibble::as_tibble(m$residuals$values_repetitions)
    m$residuals$autocorrelation$per_distance <- tibble::as_tibble(m$residuals$autocorrelation$per_distance)
    m$residuals$autocorrelation$per_repetition <- tibble::as_tibble(m$residuals$autocorrelation$per_repetition)
    m$predictions$repetitions_full <- tibble::as_tibble(m$predictions$repetitions_full)
    m$predictions$repetitions_oob <- tibble::as_tibble(m$predictions$repetitions_oob)
    m$importance$global <- tibble::as_tibble(m$importance$global)
    m$importance$per_repetition <- tibble::as_tibble(m$importance$per_repetition)
    m$importance$local <- tibble::as_tibble(m$importance$local)
  }

  #print model
  if(verbose == TRUE){
    print(m)
  }

  #return m
  m

}
