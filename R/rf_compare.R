#' @title Compares models via spatial cross-validation
#' @description Uses [rf_evaluate()] to compare the performance of several models on independent spatial folds via spatial cross-validation.
#' @param models Named list with models resulting from [rf()], [rf_spatial()], [rf_tuning()], or [rf_evaluate()]. Example: `models = list(a = model.a, b = model.b)`. Default: `NULL`
#' @param xy Data frame or matrix with two columns containing coordinates and named "x" and "y". Default: `NULL`
#' @param repetitions Integer, number of spatial folds to use during cross-validation. Must be lower than the total number of rows available in the model's data. Default: `30`
#' @param training.fraction Proportion between 0.5 and 0.9 indicating the proportion of records to be used as training set during spatial cross-validation. Default: `0.75`
#' @param metrics Character vector, names of the performance metrics selected. The possible values are: "r.squared" (`cor(obs, pred) ^ 2`), "pseudo.r.squared" (`cor(obs, pred)`), "rmse" (`sqrt(sum((obs - pred)^2)/length(obs))`), "nrmse" (`rmse/(quantile(obs, 0.75) - quantile(obs, 0.25))`). Default: `c("r.squared", "pseudo.r.squared", "rmse", "nrmse")`
#' @param distance.step Numeric, argument `distance.step` of [thinning_til_n()]. distance step used during the selection of the centers of the training folds. These fold centers are selected by thinning the data until a number of folds equal or lower than `repetitions` is reached. Its default value is 1/1000th the maximum distance within records in `xy`. Reduce it if the number of training folds is lower than expected.
#' @param distance.step.x Numeric, argument `distance.step.x` of [make_spatial_folds()]. Distance step used during the growth in the x axis of the buffers defining the training folds. Default: `NULL` (1/1000th the range of the x coordinates).
#' @param distance.step.y Numeric, argument `distance.step.x` of [make_spatial_folds()]. Distance step used during the growth in the y axis of the buffers defining the training folds. Default: `NULL` (1/1000th the range of the y coordinates).
#' @param fill.color Character vector with hexadecimal codes (e.g. "#440154FF" "#21908CFF" "#FDE725FF"), or function generating a palette (e.g. `viridis::viridis(100)`). Default: `viridis::viridis(100, option = "F", direction = -1)`
#' @param line.color Character string, color of the line produced by `ggplot2::geom_smooth()`. Default: `"gray30"`
#' @param seed Integer, random seed to facilitate reproduciblity. If set to a given number, the results of the function are always the same.
#' @param verbose Logical. If `TRUE`, messages and plots generated during the execution of the function are displayed, Default: `TRUE`
#' @param n.cores Integer, number of cores to use for parallel execution. Creates a socket cluster with `parallel::makeCluster()`, runs operations in parallel with `foreach` and `%dopar%`, and stops the cluster with `parallel::clusterStop()` when the job is done. Default: `parallel::detectCores() - 1`
#' @param cluster A cluster definition generated with `parallel::makeCluster()`. If provided, overrides `n.cores`. When `cluster = NULL` (default value), and `model` is provided, the cluster in `model`, if any, is used instead. If this cluster is `NULL`, then the function uses `n.cores` instead. The function does not stop a provided cluster, so it should be stopped with `parallel::stopCluster()` afterwards. The cluster definition is stored in the output list under the name "cluster" so it can be passed to other functions via the `model` argument, or using the `%>%` pipe. Default: `NULL`
#' @return A list with three slots:
#' \itemize{
#' \item `comparison.df`: Data frame with one performance value per spatial fold, metric, and model.
#' \item `spatial.folds`: List with the indices of the training and testing records for each evaluation repetition.
#' \item `plot`: Violin-plot of `comparison.df`.
#' }
#' @examples
#' \donttest{
#' if(interactive()){
#'
#'  data(distance_matrix)
#'  data(plant_richness_df)
#'
#'  rf.model <- rf(
#'    data = plant_richness_df,
#'    dependent.variable.name = "richness_species_vascular",
#'    predictor.variable.names = colnames(plant_richness_df)[5:21],
#'    distance.matrix = distance_matrix,
#'    distance.thresholds = c(0, 1000, 2000)
#'  )
#'
#'  rf.spatial <- rf_spatial(model = rf.model)
#'
#'comparison <- rf_compare(
#'  models = list(
#'    `Non spatial` = rf.model,
#'    Spatial = rf.spatial
#'  ),
#'  xy = plant_richness_df[, c("x", "y")],
#'  metrics = c("r.squared", "rmse")
#')
#'
#'  }
#' }
#' @seealso [rf_evaluate()]
#' @rdname rf_compare
#' @export
rf_compare <- function(
  models = NULL,
  xy = NULL,
  repetitions = 30,
  training.fraction = 0.75,
  metrics = c(
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
    alpha = 0.8
  ),
  line.color = "gray30",
  seed = NULL,
  verbose = TRUE,
  n.cores = parallel::detectCores() - 1,
  cluster = NULL
){

  #declaring variables
  model <- NULL
  value <- NULL
  model.name <- NULL
  metric <- NULL

  #capturing user options
  user.options <- options()
  #avoid dplyr messages
  options(dplyr.summarise.inform = FALSE)
  on.exit(options <- user.options)

  #testing method argument
  metrics <- match.arg(
    arg = metrics,
    choices = c("r.squared", "pseudo.r.squared", "rmse", "nrmse", "auc"),
    several.ok = TRUE
  )

  #missing input
  if(is.null(models)){
    stop("There are no models to compare in 'models'.")
  }
  if(is.null(xy)){
    stop("Case coordinates 'xy' is missing.")
  }


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

  #list to store evaluation outputs
  evaluation.list <- list()

  #iterating through models
  for(model.i in names(models)){

    #evaluating model
    models[[model.i]] <- rf_evaluate(
      model = models[[model.i]],
      xy = xy,
      repetitions = repetitions,
      training.fraction = training.fraction,
      distance.step = distance.step,
      distance.step.x = distance.step.x,
      distance.step.y = distance.step.y,
      metrics = metrics,
      seed = seed,
      verbose = FALSE,
      n.cores = NULL,
      cluster = cluster
    )

    #getting evaluation data frame
    evaluation.df.i <- models[[model.i]]$evaluation$per.fold.long
    evaluation.df.i$model.name <- model.i

    #adding it to the evaluation list
    evaluation.list[[model.i]] <- evaluation.df.i

  }#end of loop

  #binding data frames
  evaluation.df <- do.call("rbind", evaluation.list)
  rownames(evaluation.df) <- NULL

  #remove non testing models
  evaluation.df <- dplyr::filter(
    evaluation.df,
    model == "Testing"
  ) %>%
    dplyr::select(
      metric,
      value,
      model.name
    ) %>%
    dplyr::rename(
      model = model.name
    )

  #df to plot
  x <- evaluation.df
  x[x$metric == "r.squared", "metric"] <- "R squared"
  x[x$metric == "pseudo.r.squared", "metric"] <- "pseudo R squared"
  x[x$metric == "rmse", "metric"] <- "RMSE"
  x[x$metric == "nrmse", "metric"] <- "NRMSE"
  x[x$metric == "auc", "metric"] <- "AUC"

  n.models <- length(models)
  if(length(fill.color) != 1){
    if(length(fill.color) > length(n.models)){
      fill.colors.function <- grDevices::colorRampPalette(
        fill.color,
        alpha = TRUE
      )
      fill.color <- fill.colors.function(n.models)
    }
  }

  #plot
  p <- ggplot2::ggplot(data = x) +
    ggplot2::aes(
      group = model,
      y = reorder(
        model,
        value,
        FUN = stats::median
      ),
      x = value,
      fill = reorder(
        model,
        value,
        FUN = stats::median
      )
    ) +
    ggplot2::geom_violin(color = line.color) +
    ggplot2::geom_boxplot(
      notch = TRUE,
      fill = "white",
      width = 0.075,
      color = line.color
    ) +
    ggplot2::facet_wrap(
      "metric",
      scales = "free",
      drop = TRUE,
      ncol = 1
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::scale_fill_manual(values = fill.color) +
    ggplot2::labs(fill = "Model") +
    ggplot2::ggtitle(
      paste0(
        "Evaluation results on ",
        repetitions,
        " spatial folds"
      )
    ) +
    ggplot2::theme(plot.title = element_text(hjust = 0.5))

  #out list
  out.list <- list()
  out.list$comparison.df <- evaluation.df
  out.list$spatial.folds <- models[[model.i]]$evaluation$spatial.folds
  out.list$plot <- p

  if(verbose == TRUE){
    suppressMessages(print(p))
  }

  #stopping cluster
  if(stop.cluster == TRUE){
    parallel::stopCluster(cl = cluster)
  }

  out.list

}
