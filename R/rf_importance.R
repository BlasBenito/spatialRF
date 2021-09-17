#' @title Contribution of each predictor to model transferability
#' @description Evaluates the contribution of the predictors to model transferability via spatial cross-validation. The function returns the median increase or decrease in a given evaluation metric (R2, pseudo R2, RMSE, nRMSE, or AUC) when a variable is introduced in a model, by comparing and evaluating via spatial cross-validation models with and without the given variable. This function was devised to provide importance scores that would be less sensitive to spatial autocorrelation than those computed internally by random forest on the out-of-bag data. This function is experimental.
#' @param model Model fitted with [rf()] and/or [rf_spatial()]. The function doesn't work with models fitted with [rf_repeat()]. Default: `NULL`
#' @param xy Data frame or matrix with two columns containing coordinates and named "x" and "y". If `NULL`, the function will throw an error. Default: `NULL`
#' @param repetitions Integer, number of spatial folds to use during cross-validation. Must be lower than the total number of rows available in the model's data. Default: `30`
#' @param training.fraction Proportion between 0.5 and 0.9 indicating the proportion of records to be used as training set during spatial cross-validation. Default: `0.75`
#' @param metric Character, nams of the performance metric to use. The possible values are: "r.squared" (`cor(obs, pred) ^ 2`), "pseudo.r.squared" (`cor(obs, pred)`), "rmse" (`sqrt(sum((obs - pred)^2)/length(obs))`), "nrmse" (`rmse/(quantile(obs, 0.75) - quantile(obs, 0.25))`), and "auc" (only for binary responses with values 1 and 0). Default: `"r.squared"`
#' @param distance.step Numeric, argument `distance.step` of [thinning_til_n()]. distance step used during the selection of the centers of the training folds. These fold centers are selected by thinning the data until a number of folds equal or lower than `repetitions` is reached. Its default value is 1/1000th the maximum distance within records in `xy`. Reduce it if the number of training folds is lower than expected.
#' @param distance.step.x Numeric, argument `distance.step.x` of [make_spatial_folds()]. Distance step used during the growth in the x axis of the buffers defining the training folds. Default: `NULL` (1/1000th the range of the x coordinates).
#' @param distance.step.y Numeric, argument `distance.step.x` of [make_spatial_folds()]. Distance step used during the growth in the y axis of the buffers defining the training folds. Default: `NULL` (1/1000th the range of the y coordinates).
#' @param fill.color Character vector with hexadecimal codes (e.g. "#440154FF" "#21908CFF" "#FDE725FF"), or function generating a palette (e.g. `viridis::viridis(100)`). Default: `viridis::viridis(100, option = "F", direction = -1, alpha = 0.8, end = 0.9)`
#' @param line.color Character string, color of the line produced by `ggplot2::geom_smooth()`. Default: `"white"`
#' @param seed Integer, random seed to facilitate reproduciblity. If set to a given number, the results of the function are always the same. Default: `1`.
#' @param verbose Logical. If `TRUE`, messages and plots generated during the execution of the function are displayed, Default: `TRUE`
#' @param n.cores Integer, number of cores to use for parallel execution. Creates a socket cluster with `parallel::makeCluster()`, runs operations in parallel with `foreach` and `%dopar%`, and stops the cluster with `parallel::clusterStop()` when the job is done. Default: `parallel::detectCores() - 1`
#' @param cluster A cluster definition generated with `parallel::makeCluster()`. If provided, overrides `n.cores`. When `cluster = NULL` (default value), and `model` is provided, the cluster in `model`, if any, is used instead. If this cluster is `NULL`, then the function uses `n.cores` instead. The function does not stop a provided cluster, so it should be stopped with `parallel::stopCluster()` afterwards. The cluster definition is stored in the output list under the name "cluster" so it can be passed to other functions via the `model` argument, or using the `%>%` pipe. Default: `NULL`
#' @return The input model with new data in its "importance" slot. The new importance scores are included in the data frame `model$importance$per.variable`, under the column names "importance.cv" (median contribution to transferability over spatial cross-validation repetitions), "importance.cv.mad" (median absolute deviation of the performance scores over spatial cross-validation repetitions), "importance.cv.percent" ("importance.cv" expressed as a percent, taking the full model's performance as baseline), and "importance.cv.mad" (median absolute deviation of "importance.cv"). The plot is stored as "cv.per.variable.plot".
#' @examples
#' \donttest{
#' if(interactive()){
#'
#' data(plant_richness_df)
#' data(distance_matrix)
#' xy <- plant_richness_df[, c("x", "y")]
#'
#' rf.model <- rf(
#'   data = plant_richness_df,
#'   dependent.variable.name = "richness_species_vascular",
#'   predictor.variable.names = colnames(plant_richness_df)[5:21],
#'   distance.matrix = distance_matrix,
#'   distance.thresholds = c(0, 1000, 2000),
#'   xy = xy,
#'   verbose = FALSE
#' )
#'
#' rf.model <- rf_importance(rf.model)
#'
#' }
#' }
#'
#' @rdname rf_importance
#' @export
rf_importance <- function(
  model = NULL,
  xy = NULL,
  repetitions = 30,
  training.fraction = 0.75,
  metric = c(
    "r.squared",
    "pseudo.r.squared",
    "rmse",
    "nrmse",
    "auc"
  ),
  distance.step = NULL,
  distance.step.x = NULL,
  distance.step.y = NULL,
  fill.color = viridis::viridis(
    100,
    option = "F",
    direction = -1,
    alpha = 1,
    end = 0.9
  ),
  line.color = "white",
  seed = 1,
  verbose = TRUE,
  n.cores = parallel::detectCores() - 1,
  cluster = NULL
){

  #declaring variables to avoid check complaints
  fold.id <- NULL
  testing.r.squared <- NULL
  training.r.squared <- NULL
  variable <- NULL
  without <- NULL
  importance <- NULL
  testing.records <- NULL
  importance.mad <- NULL
  importance.percent <- NULL
  importance.percent.mad <- NULL
  importance.x <- NULL
  importance.y <- NULL

  #testing method argument
  metric <- match.arg(
    arg = metric,
    choices = c("r.squared", "pseudo.r.squared", "rmse", "nrmse", "auc"),
    several.ok = FALSE
  )

  #CLUSTER SETUP
  #cluster is provided
  if(!is.null(cluster)){

    #n.cores <- NULL
    n.cores <- NULL

    #flat to not stop cluster after execution
    stop.cluster <- FALSE

  } else {

    #creates and registers cluster
    cluster <- parallel::makeCluster(
      n.cores,
      type = "PSOCK"
    )

    #flag to stop cluster
    stop.cluster <- TRUE

  }

  #registering cluster
  doParallel::registerDoParallel(cl = cluster)

  #evaluating the full model
  if(verbose == TRUE){
    message("Evaluating the full model with spatial cross-validation.")
  }

  #evaluating the full model
  model <- rf_evaluate(
    model = model,
    xy = xy,
    repetitions = repetitions,
    training.fraction = training.fraction,
    distance.step = distance.step,
    distance.step.x = distance.step.x,
    distance.step.y = distance.step.y,
    metrics = metric,
    seed = seed,
    verbose = FALSE,
    n.cores = NULL,
    cluster = cluster
  )

  #getting the evaluation.df
  evaluation.df <- model$evaluation$per.fold %>%
    dplyr::select(
      fold.id,
      dplyr::contains("testing.")
    ) %>%
    dplyr::select(
      -testing.records
    )
  colnames(evaluation.df)[2] <- "with"

  #getting training data
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
    stop("nrow(xy) and nrow(data) (stored in model$ranger.arguments$data) must be the same.")
  }

  #list to store evaluation dataframes
  evaluation.list <- list()

  #evaluating the full model
  if(verbose == TRUE){
    message("Evaluating models fitted without each predictor.")
  }

  #iterating across predictors
  for(predictor.i in predictor.variable.names){

    #copy of data
    data.i <- data

    #removing predictor.i
    predictor.variable.names.i <- predictor.variable.names[predictor.variable.names != predictor.i]

    #fitting model without predictor.i
    model.i <- rf(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names.i,
      xy = xy,
      ranger.arguments = ranger.arguments,
      seed = seed,
      verbose = FALSE,
      n.cores = n.cores,
      cluster = cluster
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
        verbose = FALSE
      )

    #getting evaluation data frame
    evaluation.list[[predictor.i]] <- model.i$evaluation$per.fold %>%
      dplyr::select(
        -testing.records
      ) %>%
      dplyr::rename(
        `without` = dplyr::contains("testing.")
      ) %>%
      dplyr::left_join(
        y = evaluation.df,
        by = "fold.id"
      ) %>%
      dplyr::mutate(
        variable = predictor.i,
        .before = fold.id
      ) %>%
      dplyr::select(
        -dplyr::contains("training.")
      ) %>%
      as.data.frame()
  }

  #putting together the evaluation data frames
  importance.per.repetition <- do.call(
    "rbind",
    evaluation.list
    )
  rownames(importance.per.repetition) <- NULL

  #summary of differences
  importance.per.variable <- importance.per.repetition %>%
    dplyr::group_by(variable) %>%
    dplyr::mutate(
      importance.mad = stats::mad(with - without) %>% round(3),
      importance.percent.mad = stats::mad((with * 100 / with[1]) - (without * 100 / with[1])) %>% round(1),
      without = median(without),
      with = median(with),
      importance = with - without,
      importance.percent = (importance * 100 / with[1]) %>% round(1)
    ) %>%
    dplyr::slice(1) %>%
    dplyr::arrange(
      dplyr::desc(importance)
    ) %>%
    dplyr::transmute(
      variable,
      with,
      without,
      importance,
      importance.mad,
      importance.percent,
      importance.percent.mad
    ) %>%
    as.data.frame()

  #pretty metric name
  if(metric == "r.squared"){
    metric.pretty <- "R-squared"
  }
  if(metric == "pseudo.r.squared"){
    metric.pretty <- "pseudo R-squared"
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

  importance.per.variable.plot <- ggplot2::ggplot(data = importance.per.variable) +
    ggplot2::aes(
      x = importance,
      y = reorder(
        variable,
        importance,
        FUN = max
      ),
      fill = importance
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "gray50"
      ) +
    ggplot2::geom_linerange(
      ggplot2::aes(
        xmin = importance - importance.mad,
        xmax = importance + importance.mad,
        color = importance
      ),
      size = 1
    ) +
    ggplot2::geom_point(
      size = 4,
      shape = 21,
      color = line.color
    ) +
    ggplot2::scale_x_continuous(
      sec.axis = ggplot2::sec_axis(~ . * 100 / importance.per.variable$with[1], name = "Percentage")
    ) +
    ggplot2::scale_fill_gradientn(colors = fill.color) +
    ggplot2::scale_color_gradientn(colors = fill.color) +
    ggplot2::ylab("") +
    ggplot2::xlab(paste0(metric.pretty)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle(
      paste0(
        "Median contribution to model transferability\n    across ",
        repetitions,
        " cross-validation repetitions"
        )
      )

  #adding to the model's importance slot
  model$importance$per.variable <- dplyr::left_join(
    x = model$importance$per.variable,
    y = importance.per.variable,
    by = "variable"
  ) %>%
    dplyr::rename(
      importance.oob = importance.x,
      importance.cv = importance.y,
      importance.cv.mad = importance.mad,
      importance.cv.percent = importance.percent,
      importance.cv.percent.mad = importance.percent.mad
      ) %>%
    dplyr::select(
      -with,
      -without
    )

  #changing names
  model$importance$oob.per.variable.plot <- model$importance$per.variable.plot
  model$importance$per.variable.plot <- NULL
  model$importance$cv.per.variable.plot <- importance.per.variable.plot

  if(verbose == TRUE){
    message("Importance scores stored in model$importance$per.variable.")
    message("Importance plot stored in model$importance$cv.per.variable.plot.")
  }

  if(verbose == TRUE){
    print(importance.per.variable.plot)
  }

  model

}




