#' @title Jackknife test of variable importance via spatial cross-validation
#' @description Fits and evaluates (via spatial cross-validation with [rf_evaluate()]) models with and without each predictor to compute a jackknife-based importance score. The predictors are ranked according to the difference in performance between models fitted only with (univariate models) and without the predictor (all predictors but the given one).
#' @param model Model fitted with [rf()] and/or [rf_spatial()]. The function doesn't work with models fitted with [rf_repeat()]. Default: `NULL`
#' @param xy Data frame or matrix with two columns containing coordinates and named "x" and "y". If `NULL`, the function will throw an error. Default: `NULL`
#' @param repetitions Integer, number of spatial folds to use during cross-validation. Must be lower than the total number of rows available in the model's data. Default: `30`
#' @param training.fraction Proportion between 0.5 and 0.9 indicating the proportion of records to be used as training set during spatial cross-validation. Default: `0.75`
#' @param metrics Character vector, names of the performance metrics selected. The possible values are: "r.squared" (`cor(obs, pred) ^ 2`), "rmse" (`sqrt(sum((obs - pred)^2)/length(obs))`), "nrmse" (`rmse/(quantile(obs, 0.75) - quantile(obs, 0.25))`), and "auc" (added automatically for binary responses with values 1 and 0). Default: `c("r.squared", "rmse", "nrmse", "auc")`
#' @param distance.step Numeric, argument `distance.step` of [thinning_til_n()]. distance step used during the selection of the centers of the training folds. These fold centers are selected by thinning the data until a number of folds equal or lower than `repetitions` is reached. Its default value is 1/1000th the maximum distance within records in `xy`. Reduce it if the number of training folds is lower than expected.
#' @param distance.step.x Numeric, argument `distance.step.x` of [make_spatial_folds()]. Distance step used during the growth in the x axis of the buffers defining the training folds. Default: `NULL` (1/1000th the range of the x coordinates).
#' @param distance.step.y Numeric, argument `distance.step.x` of [make_spatial_folds()]. Distance step used during the growth in the y axis of the buffers defining the training folds. Default: `NULL` (1/1000th the range of the y coordinates).
#' @param fill.color Character vector with hexadecimal codes (e.g. "#440154FF" "#21908CFF" "#FDE725FF"), or function generating a palette (e.g. `viridis::viridis(100)`). Default: `viridis::viridis(100, option = "F", direction = -1, alpha = 0.8, end = 0.9)`
#' @param seed Integer, random seed to facilitate reproduciblity. If set to a given number, the results of the function are always the same. Default: `1`.
#' @param verbose Logical. If `TRUE`, messages and plots generated during the execution of the function are displayed, Default: `TRUE`
#' @param n.cores Integer, number of cores used by \code{\link[ranger]{ranger}} for parallel execution (used as value for the argument `num.threads` in `ranger()`). Default: `parallel::detectCores() - 1`
#' @param cluster A cluster definition generated with `parallel::makeCluster()` or \code{\link{start_cluster}}. Faster than using `n.cores` for smaller models. If provided, overrides `n.cores`. The function does not stop a cluster, please remember to shut it down with `parallel::stopCluster(cl = cluster_name)` or \code{\link{stop_cluster}} at the end of your pipeline. Default: `NULL`
#' @details Model evaluation is based on spatial cross-validation. If the response is numeric, the R-squared is used, but if the response is binary (with values 1 and 0), then AUC is used instead.
#' @return The input model with new slot named "jackknife". This is a list with slots named after the metrics introduced in the argument `metrics`. For example, if one of the metrics used is "r.squared", then the plot of this metric will be in `model$jackknife$r.squared$plot`, and the dataframe used to build the plot will be in `model$jackknife$r.squared$df`.
#' @examples
#' if(interactive()){
#'
#' #loading example data
#' data(
#'   ecoregions_df,
#'   ecoregions_distance_matrix,
#'   ecoregions_predictor_variable_names,
#'   ecoregions_dependent_variable_name
#'   )
#'
#'   cluster <- start_cluster()
#'
#' #fitting random forest model
#' rf.model <- rf(
#'   data = ecoregions_df,
#'   dependent.variable.name = ecoregions_dependent_variable_name,
#'   predictor.variable.names = ecoregions_predictor_variable_names,
#'   distance.matrix = ecoregions_distance_matrix,
#'   distance.thresholds = 0,
#'   xy = ecoregions_df[, c("x", "y")],
#'   n.cores = 1,
#'   verbose = FALSE
#' )
#'
#' #computing predictor contribution to model transferability
#' rf.model <- rf_jackknife(
#'   model = rf.model,
#'   cluster = cluster,
#'   verbose = TRUE
#'   )
#'
#' stop_cluster(cluster)
#'
#' #accessing results
#' rf.model$jackknife$r.squared$df
#' rf.model$jackknife$r.squared$plot
#'
#' }
#'
#' @rdname rf_jackknife
#' @export
rf_jackknife <- function(
  model = NULL,
  xy = NULL,
  repetitions = 30,
  training.fraction = 0.75,
  metrics = c(
    "r.squared",
    "rmse",
    "nrmse",
    "auc"
  ),
  distance.step = NULL,
  distance.step.x = NULL,
  distance.step.y = NULL,
  fill.color = viridis::viridis(
    3,
    option = "F",
    direction = 1,
    alpha = 1,
    end = 0.9
  ),
  seed = 1,
  verbose = TRUE,
  n.cores = parallel::detectCores() - 1,
  cluster = NULL
){

  #declaring variables to avoid check complaints
  only.with <- NULL
  without.median <- NULL
  only.with.median <- NULL
  without.minus.only.with <- NULL
  full.median <- NULL
  group <- NULL
  variable_name <- NULL
  without <- NULL
  variable <- NULL
  testing.records <- NULL
  fold.id <- NULL

  #terminating if there is no model
  if(is.null(model)){
    stop("The argument 'model' is empty, there is no model to work with.")
  }

  #stopping if model is not of the right class
  if(!("rf" %in% class(model))){
    stop("The argument 'model' is not of the class 'rf'.")
  }


  #getting data and ranger arguments from the model
  data <- model$ranger.arguments$data
  dependent.variable.name <- model$ranger.arguments$dependent.variable.name
  predictor.variable.names <- model$ranger.arguments$predictor.variable.names
  ranger.arguments <- model$ranger.arguments
  ranger.arguments$data <- NULL
  ranger.arguments$dependent.variable.name <- NULL
  ranger.arguments$predictor.variable.names <- NULL
  ranger.arguments$importance <- "none"
  ranger.arguments$local.importance <- FALSE
  ranger.arguments$data <- NULL
  ranger.arguments$scaled.importance <- FALSE
  ranger.arguments$distance.matrix <- NULL

  #getting xy
  if(is.null(xy)){
    if(is.null(model$ranger.arguments$xy)){
      stop("The argument 'xy' is required for spatial cross-validation.")
    } else {
      xy <- model$ranger.arguments$xy
    }
  }

  if(sum(c("x", "y") %in% colnames(xy)) < 2){
    stop("The column names of 'xy' must be 'x' and 'y'.")
  }

  if(nrow(xy) != nrow(data)){
    stop("nrow(xy) and nrow(data) must be the same.")
  }

  #if data is binary, "auc" is added
  if(.is_binary(
    x = dplyr::pull(data, dependent.variable.name)
  )){
    metrics <- c(metrics, "auc")
  } else {
    metrics <- metrics[metrics != "auc"]
  }
  metrics <- unique(metrics)


  #list to store evaluation dataframes
  evaluation.list <- list()

  #evaluating the full model
  if(verbose == TRUE){

    message("Evaluating the full model with spatial cross-validation.\n")

  }

  #evaluating full model
  model <- rf_evaluate(
      model = model,
      repetitions = repetitions,
      training.fraction = training.fraction,
      distance.step = distance.step,
      distance.step.x = distance.step.x,
      distance.step.y = distance.step.y,
      metrics = metrics,
      seed = seed,
      verbose = FALSE,
      n.cores = n.cores,
      cluster = cluster
    )

  #evaluating the full model
  if(verbose == TRUE){
    message("Fitting and evaluating univariate models with each predictor and multivariate models without each predictor.\n")
  }

  #iterating over predictors
  for(predictor.i in predictor.variable.names){

    #model with only the variable

    #generating training data
    training.df.without.i <- data.frame(
      y = data[, dependent.variable.name],
      x1 = data[, predictor.i],
      x2 = data[, predictor.i],
      x3 = data[, predictor.i]
    )

    #evaluating the model with only the variable
    model.only.with <- rf(
      data = training.df.without.i,
      dependent.variable.name = "y",
      predictor.variable.names = c("x1", "x2", "x3"),
      xy = xy,
      ranger.arguments = ranger.arguments,
      seed = seed,
      verbose = FALSE,
      n.cores = n.cores
    ) %>%
    rf_evaluate(
      xy = xy,
      repetitions = repetitions,
      training.fraction = training.fraction,
      distance.step = distance.step,
      distance.step.x = distance.step.x,
      distance.step.y = distance.step.y,
      metrics = metrics,
      seed = seed,
      verbose = FALSE,
      n.cores = n.cores,
      cluster = cluster
    )

    #getting the evaluation.df
    evaluation.df.only.with <- model.only.with$evaluation$per.fold %>%
      dplyr::select(
        fold.id,
        dplyr::contains("testing.")
      ) %>%
      dplyr::select(
        -testing.records
      )

    #model without the variable

    #removing predictor.i
    predictor.variable.names.i <- predictor.variable.names[predictor.variable.names != predictor.i]

    #fitting model without predictor.i
    model.without <- rf(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names.i,
      xy = xy,
      ranger.arguments = ranger.arguments,
      seed = seed,
      verbose = FALSE,
      n.cores = n.cores
    ) %>%
      rf_evaluate(
        repetitions = repetitions,
        xy = xy,
        training.fraction = training.fraction,
        distance.step = distance.step,
        distance.step.x = distance.step.x,
        distance.step.y = distance.step.y,
        metrics = metrics,
        seed = seed,
        verbose = FALSE,
        n.cores = n.cores,
        cluster = cluster
      )

    #getting the evaluation.df
    evaluation.df.without <- model.without$evaluation$per.fold %>%
      dplyr::select(
        fold.id,
        dplyr::contains("testing.")
      ) %>%
      dplyr::select(
        -testing.records
      )

    #getting evaluation data frame
    evaluation.df <- evaluation.df.without %>%
      dplyr::left_join(
        y = evaluation.df.only.with,
        by = "fold.id"
      ) %>%
      dplyr::mutate(
        variable = predictor.i,
        .before = fold.id
      ) %>%
      na.omit() %>%
      as.data.frame()

    names(evaluation.df) <- gsub(
      pattern = ".x",
      replacement = ".without",
      x = names(evaluation.df)
    )

    names(evaluation.df) <- gsub(
      pattern = ".y",
      replacement = ".only.with",
      x = names(evaluation.df)
    )

    names(evaluation.df) <- gsub(
      pattern = "testing.",
      replacement = "",
      x = names(evaluation.df)
    )

    evaluation.list[[predictor.i]] <- evaluation.df

  }#end of iterations

  #putting together the evaluation data frames
  importance.per.repetition <- do.call(
    "rbind",
    evaluation.list
    )
  rownames(importance.per.repetition) <- NULL


  #iterating over metrics
  ########################
  jackknife.list <- list()

  for(metric in metrics){

    #summary of differences
    importance.per.variable <- importance.per.repetition %>%
      dplyr::select(
        variable,
        fold.id,
        dplyr::contains(metric)
      ) %>%
      dplyr::group_by(variable) %>%
      dplyr::summarise(
        without.median = stats::median(
          !!rlang::sym(
            paste0(
              metric,
              ".without"
              )
            )
          ),
        only.with.median = stats::median(
          !!rlang::sym(
            paste0(
              metric,
              ".only.with"
              )
            )
          ),
        without.minus.only.with = without.median -  only.with.median
      ) %>%
      dplyr::ungroup()

    #arranging
    if(metric %in% c("r.squared", "auc")){

      importance.per.variable <- dplyr::arrange(
        importance.per.variable,
        dplyr::desc(without.minus.only.with)
      ) %>%
        dplyr::mutate(
          full.median = median(model$evaluation$per.fold[, paste0("testing.", metric)]),
          variable_name = ifelse(
            without.median > full.median,
            paste(variable, "*"),
            variable
          )
        )

      #minimum value in x axis
      plot.x.min <- 0
      plot.x.max <- max(importance.per.variable[, c("without.median", "only.with.median", "full.median")])

    } else {

      importance.per.variable <- dplyr::arrange(
        importance.per.variable,
        without.minus.only.with
      ) %>%
        dplyr::mutate(
          full.median = median(model$evaluation$per.fold[, paste0("testing.", metric)]),
          variable_name = ifelse(
            without.median < full.median,
            paste(variable, "*"),
            variable
          )
        )

      #minimum value in x axis
      plot.x.min <- min(importance.per.variable$without.median) - (min(importance.per.variable$without.median)/10)
      plot.x.max <- max(importance.per.variable[, c("without.median", "only.with.median", "full.median")])

    }

    #to long format for plotting
    importance.per.variable.long <- importance.per.variable %>%
      tidyr::pivot_longer(
        cols = c("without.median", "only.with.median"),
        names_to = "group",
        values_to = "median"
      ) %>%
      dplyr::mutate(
        group = gsub(
          pattern = ".median",
          replacement = "",
          x = group
        ),
        group = gsub(
          pattern = "\\.",
          replacement = " ",
          x = group
        )
      ) %>%
      dplyr::select(
        variable,
        variable_name,
        group,
        median
      )

    #creating the line for the full model
    full.model.line <- data.frame(
      variable = "full_model",
      variable_name = "full_model",
      group = "full",
      median = importance.per.variable$full.median[1]
    )

    #adding the full model
    importance.per.variable.long <- rbind(
      importance.per.variable.long,
      full.model.line
    ) %>%
      dplyr::mutate(
        variable_name = factor(
          x = variable_name,
          levels = c(importance.per.variable$variable_name, "full_model")
        )
      )

    #pretty metric name
    if(metric == "r.squared"){
      metric.pretty <- "R-squared"
    }
    if(metric == "rmse"){
      metric.pretty <- "RMSE"
    }
    if(metric == "nrmse"){
      metric.pretty <- "normalized RMSE"
    }
    if(metric == "auc"){
      metric.pretty <- "AUC"
    }

    jackknife.plot <- ggplot2::ggplot(data = importance.per.variable.long) +
      ggplot2::aes(
        y = variable_name,
        x = median,
        fill = group
      ) +
      ggplot2::geom_bar(
        position = "dodge",
        stat = "identity"
      ) +
      ggplot2::scale_fill_manual(values = fill.color[1:3]) +
      ggplot2::labs(
        x = paste0(
          "Median ",
          metric.pretty,
          " on ",
          repetitions,
          " spatial folds"
        ),
        y = "",
        fill = "Model",
        title = "Model performance with and without each predictor",
        subtitle = "Note: predictors marked with * decrease model performance."
      ) +
      ggplot2::theme_bw() +
      ggplot2::coord_cartesian(
        xlim = c(plot.x.min, plot.x.max),
        expand = FALSE
        ) +
      ggplot2::geom_vline(
        xintercept = full.model.line$median,
        color = fill.color[1]
      )

    #storing results
    jackknife.df <- importance.per.variable %>%
      dplyr::select(
        variable,
        without.median,
        only.with.median,
        full.median
      ) %>%
      as.data.frame()

    #renaming
    names(jackknife.df)[2:4] <- c(
      paste0(
        metric,
        ".without"
      ),
      paste0(
        metric,
        ".only.with"
      ),
      paste0(
        metric,
        ".all"
      )
    )

    names(jackknife.df) <- gsub(
      pattern = "\\.",
      replacement = "_",
      x = names(jackknife.df)
    )

    #saving in jackknife.list
    jackknife.list[[metric]]$df <- jackknife.df
    jackknife.list[[metric]]$plot <- jackknife.plot

  }

  model$jackknife <- jackknife.list

  if(verbose == TRUE){
    message(
      paste0(
        "Jackknife plots stored in: \n",
          paste0(
            "  - model$jackknife$" ,
            metrics,
            "$plot",
            collapse = "\n"
            )
          )
      )
    message(
      paste0(
        "\nJackknife dataframes stored in: \n",
        paste0(
          "  - model$jackknife$" ,
          metrics,
          "$df",
          collapse = "\n"
        )
      )
    )

    message(
      paste0(
        "Plotting result for metric '",
        metrics[1],
        "':"
      )
    )

    print(jackknife.list[[metrics[1]]]$plot)

    message(
      "Interpretation:\n",
      "---------------\n\n",
      " - 'full' represents the median performance of a model fitted with all predictors.\n\n",
      " - 'only with' represents the median performance of a model fitted with the given predictor alone (univariate model).\n\n",
      " - 'without' represents the median performance of a model fitted with all predictors but the one in the y axis.\n\n",
      " - Predictors are ranked by the maximum difference between their 'only with' and 'without'.\n\n",
      " - Predictors marked with * decrease model performance (on spatial cross-validation) when included in the model."
      )

  }

  model

}


