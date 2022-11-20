#' @title Fits several random forest models on the same data
#' @description Fits several random forest models on the same data in order to capture the effect of the algorithm's stochasticity on the variable importance scores, predictions, residuals, and performance measures. The function relies on the median to aggregate performance and importance values across repetitions. It is recommended to use it after a model is fitted ([rf()] or [rf_spatial()]), tuned ([rf_tuning()]), and/or evaluated ([rf_evaluate()]). This function is designed to be used after fitting a model with [rf()] or [rf_spatial()], tuning it with [rf_tuning()] and evaluating it with [rf_evaluate()].
#' @param model (required if `data` is `NULL`; model produced with `spatialRF`) A model fitted with [rf()]. If provided, the data and ranger arguments are taken directly from the model definition (stored in `model$ranger.arguments`), and the argument `ranger.arguments` is ignored. Default: `NULL`
#' @param data (required if `model` is `NULL`; data frame or tibble) Data frame with a response variable and a set of predictors. If `data` is a tibble, all data frames in the output model are coerced to tibble. Default: `NULL`
#' @param dependent.variable.name (required if `model` is `NULL`; character string) Character string with the name of the response variable. Must be in the column names of `data`. If the dependent variable is binary with values 1 and 0, the argument `case.weights` of `ranger` is populated by the function [case_weights()]. Default: `NULL`
#' @param predictor.variable.names (required if `model` is `NULL`; character vector with column names of `data`) Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Default: `NULL`
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
#'     \item `rsquared.oob`: R-squared computed on the out-of-bag predictions using the expression `cor(observed, predicted.oob) ^ 2`.
#'     \item `rmse.oob`: Root mean squared error computed on the out-of-bag predictions using the expression `spatialRF::root_mean_squared_error(o = observed, p = predicted.oob)`.
#'     \item `nrmse.oob`: Normalized rood mean squared error computed on the out-of-bag data using the expression `spatialRF::root_mean_squared_error(o = observed, p = predicted.oob, normalization = "iq")`.
#'     \item `auc.oob`: Only for binary responses with values 0 and 1. Area under the ROC curve computed on the out-of-bag predictions using the expression `spatialRF::auc(o = observed, p = predicted.oob)`.
#'     \item `rsquared.ib`: R-squared computed on the in-bag predictions using the expression `cor(observed, predicted.ib) ^ 2`.
#'     \item `rmse.ib`: Root mean squared error computed on the in-bag predictions using the expression `spatialRF::root_mean_squared_error(o = observed, p = predicted.ib)`.
#'     \item `nrmse.ib`: Normalized rood mean squared error computed on the in-bag data using the expression `spatialRF::root_mean_squared_error(o = observed, p = predicted.ib, normalization = "iq")`.
#'     \item `auc.ib`: Only for binary responses with values 0 and 1. Area under the ROC curve computed on the in-bag predictions using the expression `spatialRF::auc(o = observed, p = predicted.ib)`.
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
#'   ecoregions_predictor_variable_names,
#'   ecoregions_dependent_variable_name
#'   )
#'
#'  #fitting 10 random forest models
#'  out <- rf_repeat(
#'    data = ecoregions_df,
#'    dependent.variable.name = ecoregions_dependent_variable_name,
#'    predictor.variable.names = ecoregions_predictor_variable_names,
#'    distance.matrix = ecoregions_distance_matrix,
#'    distance.thresholds = 0,
#'    repetitions = 10,
#'    n.cores = 1
#'  )
#'
#'  #data frame with median variable importance
#'  out$importance$per.variable
#'
#'  #importance per repetition
#'  out$importance$per.repetition
#'
#'  #variable importance plot
#'  out$importance$per.repetition.plot
#'
#'  #also
#'  plot_importance(out)
#'
#'  #performance per repetition
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
#'    data = ecoregions_df,
#'    dependent.variable.name = ecoregions_dependent_variable_name,
#'    predictor.variable.names = ecoregions_predictor_variable_names,
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
#'  rf.repeat$importance$per.repetition.plot
#'
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
      dependent.variable.name <- NULL
      predictor.variable.names <- NULL
      distance.matrix <- NULL
      distance.thresholds <- NULL
      xy <- NULL

      #writing the model's ranger.arguments to the environment
      ranger.arguments <- model$ranger.arguments

      #writing arguments to the function environment
      list2env(model$ranger.arguments, envir=environment())

    } else {

      #RULE 2:
      #input arguments in model$ranger.arguments take precedence

      ranger.arguments$data <- NULL
      ranger.arguments$dependent.variable.name <- NULL
      ranger.arguments$predictor.variable.names <- NULL
      ranger.arguments$distance.matrix <- NULL
      ranger.arguments$distance.thresholds <- NULL
      ranger.arguments$xy <- NULL

      #writing arguments to the function environment
      list2env(model$ranger.arguments, envir=environment())
      list2env(ranger.arguments, envir=environment())


    }

  } else {
    #RULE 3:

    if(!is.null(ranger.arguments)){

      if(is.null(data)){
        data <- model$ranger.arguments$data
      }

      if(is.null(dependent.variable.name)){
        dependent.variable.name <- model$ranger.arguments$dependent.variable.name
      }

      if(is.null(predictor.variable.names)){
        predictor.variable.names <- model$ranger.arguments$predictor.variable.names
      }

      if(is.null(distance.matrix)){
        distance.matrix <- model$ranger.arguments$distance.matrix
      }

      if(is.null(distance.thresholds)){
        distance.thresholds <- model$ranger.arguments$distance.thresholds
      }

      if(is.null(xy)){
        xy <- model$ranger.arguments$xy
      }

      if(is.null(cluster)){
        cluster <- model$ranger.arguments$cluster
      }

      if(is.null(seed)){
        seed <- model$ranger.arguments$seed
      }

      #writing ranger.arguments to the function environment
      list2env(ranger.arguments, envir=environment())

    }

  }

  #predictor.variable.names comes from auto_vif or auto_cor
  if(inherits(predictor.variable.names, "variable_selection")){

    predictor.variable.names <- predictor.variable.names$selected.variables

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
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
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
    out$importance$local <- m.i$variable.importance.local
    out$importance <- m.i$importance$per.variable
    out$importance.local <- as.matrix(m.i$importance$local)
    out$prediction.error <- m.i$prediction.error
    out$rsquared.ib <- m.i$performance$rsquared.ib
    out$rsquared.oob <- m.i$performance$rsquared.oob
    out$rmse.ib <- m.i$performance$rmse.ib
    out$rmse.oob <- m.i$performance$rmse.oob
    out$nrmse.ib <- m.i$performance$nrmse.ib
    out$nrmse.oob <- m.i$performance$nrmse.oob
    out$auc.ib <- m.i$performance$auc.ib
    out$auc.oob <- m.i$performance$auc.oob
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
  predictions.per.repetition.ib <- as.data.frame(
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
  colnames(predictions.per.repetition.ib) <- repetition.columns

  predictions.per.repetition.oob <- as.data.frame(
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
  colnames(predictions.per.repetition.oob) <- repetition.columns

  #computing medians
  # predictions.ib.median <- data.frame(
  #   median = apply(
  #     predictions.per.repetition.ib,
  #     1,
  #     FUN = median
  #     ),
  #   median_absolute_deviation = apply(
  #     predictions.per.repetition.ib,
  #     1, stats::mad
  #     ),
  #   row.names = NULL
  # )
  #
  # predictions.oob.median <- data.frame(
  #   median = apply(
  #     predictions.per.repetition.oob,
  #     1,
  #     FUN = median
  #   ),
  #   median_absolute_deviation = apply(
  #     predictions.per.repetition.oob,
  #     1, stats::mad
  #   ),
  #   row.names = NULL
  # )

  m$predictions <- NULL
  m$predictions$ib <- apply(
    predictions.per.repetition.ib,
    1,
    FUN = median
  )
  m$predictions$oob <- apply(
    predictions.per.repetition.oob,
    1,
    FUN = median
  )
  m$predictions$ib.repetitions <- predictions.per.repetition.ib
  m$predictions$oob.repetitions <- predictions.per.repetition.oob



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
    model = importance.per.variable,
    verbose = FALSE
  )

  if(repetitions > 1){
    m$importance$per.repetition <- importance.per.repetition
  }

  #violin plot only if repetitions equal or larger than 10
  if(repetitions >= 10){
    m$importance$per.repetition.plot <- plot_importance(
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

  m$performance$rsquared.oob <- unlist(
    lapply(
      repeated.models,
      "[[",
      "rsquared.oob"
    )
  )

  m$performance$rmse.oob <- unlist(
    lapply(
      repeated.models,
      "[[",
      "rmse.oob"
    )
  )
  names(m$performance$rmse.oob) <- NULL

  m$performance$nrmse.oob <- unlist(
    lapply(
      repeated.models,
      "[[",
      "nrmse.oob"
    )
  )
  names(m$performance$nrmse.oob) <- NULL

  m$performance$auc.oob <- unlist(
    lapply(
      repeated.models,
      "[[",
      "auc.oob"
    )
  )
  names(m$performance$auc.oob) <- NULL

  m$performance$rsquared.ib <- unlist(
    lapply(
      repeated.models,
      "[[",
      "rsquared.ib"
    )
  )

  m$performance$rmse.ib <- unlist(
    lapply(
      repeated.models,
      "[[",
      "rmse.ib"
    )
  )
  names(m$performance$rmse.ib) <- NULL

  m$performance$nrmse.ib <- unlist(
    lapply(
      repeated.models,
      "[[",
      "nrmse.ib"
    )
  )
  names(m$performance$nrmse.ib) <- NULL

  m$performance$auc.ib <- unlist(
    lapply(
      repeated.models,
      "[[",
      "auc.ib"
    )
  )
  names(m$performance$auc.ib) <- NULL



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
    predictions = m$predictions$ib
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
      m$tuning <- model$tuning
    }

    if("evaluation" %in% names(model)){
      m$evaluation <- model$evaluation
    }

    if("variable.selection" %in% names(model)){
      m$variable.selection <- model$variable.selection
    }
  }

  #adding class to the model
  class(m) <- unique(c(class(m), "rf_repeat", "ranger"))

  #adding cluster to model
  if(!is.null(cluster) & "cluster" %in% class(cluster)){
    m$ranger.arguments$cluster <- cluster
  }

  #coercing output to tibble
  if(return.tibble == TRUE){
    m$variable.importance.local <- tibble::as_tibble(m$variable.importance.local)
    m$residuals$values.median <- tibble::as_tibble(m$residuals$values.median)
    m$residuals$values.repetitions <- tibble::as_tibble(m$residuals$values.repetitions)
    m$residuals$autocorrelation$per.distance <- tibble::as_tibble(m$residuals$autocorrelation$per.distance)
    m$residuals$autocorrelation$per.repetition <- tibble::as_tibble(m$residuals$autocorrelation$per.repetition)
    m$predictions$ib.repetitions <- tibble::as_tibble(m$predictions$ib.repetitions)
    m$predictions$oob.repetitions <- tibble::as_tibble(m$predictions$oob.repetitions)
    m$importance$per.variable <- tibble::as_tibble(m$importance$per.variable)
    m$importance$per.repetition <- tibble::as_tibble(m$importance$per.repetition)
    m$importance$local <- tibble::as_tibble(m$importance$local)
  }

  #print model
  if(verbose == TRUE){
    print(m)
  }

  #return m
  m

}
