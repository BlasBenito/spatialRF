#' @title Jackknife test of variable importance
#' @description Performs a jackknife test fitting and evaluating (via spatial cross-validation with [rf_evaluate()]) models with and without each predictor. This function was devised to provide importance scores that would be less sensitive to spatial autocorrelation than those computed internally by random forest on the out-of-bag data. This function is experimental.
#' @param model Model fitted with [rf()] and/or [rf_spatial()]. The function doesn't work with models fitted with [rf_repeat()]. Default: `NULL`
#' @param xy Data frame or matrix with two columns containing coordinates and named "x" and "y". If `NULL`, the function will throw an error. Default: `NULL`
#' @param repetitions Integer, number of spatial folds to use during cross-validation. Must be lower than the total number of rows available in the model's data. Default: `30`
#' @param training.fraction Proportion between 0.5 and 0.9 indicating the proportion of records to be used as training set during spatial cross-validation. Default: `0.75`
#' @param distance.step Numeric, argument `distance.step` of [thinning_til_n()]. distance step used during the selection of the centers of the training folds. These fold centers are selected by thinning the data until a number of folds equal or lower than `repetitions` is reached. Its default value is 1/1000th the maximum distance within records in `xy`. Reduce it if the number of training folds is lower than expected.
#' @param distance.step.x Numeric, argument `distance.step.x` of [make_spatial_folds()]. Distance step used during the growth in the x axis of the buffers defining the training folds. Default: `NULL` (1/1000th the range of the x coordinates).
#' @param distance.step.y Numeric, argument `distance.step.x` of [make_spatial_folds()]. Distance step used during the growth in the y axis of the buffers defining the training folds. Default: `NULL` (1/1000th the range of the y coordinates).
#' @param fill.color Character vector with hexadecimal codes (e.g. "#440154FF" "#21908CFF" "#FDE725FF"), or function generating a palette (e.g. `viridis::viridis(100)`). Default: `viridis::viridis(100, option = "F", direction = -1, alpha = 0.8, end = 0.9)`
#' @param seed Integer, random seed to facilitate reproduciblity. If set to a given number, the results of the function are always the same. Default: `1`.
#' @param verbose Logical. If `TRUE`, messages and plots generated during the execution of the function are displayed, Default: `TRUE`
#' @param n.cores Integer, number of cores to use for parallel execution. Default: `NULL`
#' @param cluster A cluster definition generated with `parallel::makeCluster()`. If provided, it overrides `n.cores`. If `NULL` but `model` has a cluster object in the "cluster" slot, then the model's cluster is used. Please notice that the function does not stop a running cluster, so it should be stopped with `parallel::stopCluster()` afterwards (). The cluster definition is stored in the output list under the name "cluster" so it can be passed to other functions via the `model` argument. Default: `parallel::makeCluster(nnodes = parallel::detectCores() - 1, type = "PSOCK")`
#' @details Model evaluation is based on spatial cross-validation. If the response is numeric, the R-squared is used, but if the response is binary (with values 1 and 0), then AUC is used instead.
#' @return The input model with new data in its "importance" slot. The jackknife data frame can be found in `model$importance$jackknife.df`, and the plot in `model$importance$jackknife.plot`.
#' @examples
#' if(interactive()){
#'
#' #loading example data
#' data(
#'   ecoregions_df,
#'   ecoregions_distance_matrix,
#'   ecoregions_predvar_names,
#'   ecoregions_depvar_name
#'   )
#'
#'   cluster <- make_cluster()
#'
#' #fitting random forest model
#' rf.model <- rf(
#'   data = ecoregions_df,
#'   dependent.variable.name = ecoregions_depvar_name,
#'   predictor.variable.names = ecoregions_predvar_names,
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
#' rf.model$importance$jackknife.df
#' rf.model$importance$jackknife.plot
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

  #evaluating the full model
  if(verbose == TRUE){
    message("Evaluating the full model with spatial cross-validation.")
  }


  if(is.null(model)){

    stop("The argument 'model' is empty, there is no model to evaluate.")

  } else {

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

  }

  if(sum(c("x", "y") %in% colnames(xy)) < 2){
    stop("The column names of 'xy' must be 'x' and 'y'.")
  }

  if(nrow(xy) != nrow(data)){
    stop("nrow(xy) and nrow(data) must be the same.")
  }

  #if data is binary, "auc" is used
  if(.is_binary(
    x = data[, dependent.variable.name]
  )){
    metric <- "auc"
  } else {
    metric <- "r.squared"
  }


  #list to store evaluation dataframes
  evaluation.list <- list()

  #evaluating the full model
  if(verbose == TRUE){
    message("Evaluating models fitted without each predictor.")
  }

  #evaluating full model
  model <- rf_evaluate(
      model = model,
      repetitions = repetitions,
      training.fraction = training.fraction,
      distance.step = distance.step,
      distance.step.x = distance.step.x,
      distance.step.y = distance.step.y,
      metrics = metric,
      seed = seed,
      verbose = FALSE,
      n.cores = n.cores,
      cluster = cluster
    )

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
      metrics = metric,
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
    colnames(evaluation.df.only.with)[2] <- "only.with"

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
        metrics = metric,
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
    colnames(evaluation.df.without)[2] <- "without"

    #getting evaluation data frame
    evaluation.list[[predictor.i]] <- evaluation.df.without %>%
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

  }#end of iterations

  #putting together the evaluation data frames
  importance.per.repetition <- do.call(
    "rbind",
    evaluation.list
    )
  rownames(importance.per.repetition) <- NULL

  #summary of differences
  importance.per.variable <- importance.per.repetition %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(
      without.median = stats::median(without),
      only.with.median = stats::median(only.with),
      without.minus.only.with = without.median -  only.with.median
    ) %>%
    dplyr::arrange(dplyr::desc(without.minus.only.with)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
        full.median = median(model$evaluation$per.fold[, paste0("testing.", metric)]),
      variable_name = ifelse(
        without.median > full.median,
        paste(variable, "***"),
        variable
      )
    )

  #variable order
  variable.order <- importance.per.variable$variable_name

  #to long format
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
        levels = c(variable.order, "full_model")
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
      subtitle = "Note: removing predictors marked with *** may increase model transferability."
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
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

   #adding results to the model
   model$importance$jackknife.df <- jackknife.df
   model$importance$jackknife.plot <- jackknife.plot

  if(verbose == TRUE){
    message("Jackknife results stored in model$importance$jackknife.df")
    message("Importance plot stored in model$importance$jackknife.plot.")

    print(jackknife.plot)
    print(jackknife.df)
  }

  model

}


