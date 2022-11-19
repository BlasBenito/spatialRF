#' @title Compares models via spatial cross-validation
#' @description Uses [rf_evaluate()] to compare the performance of several models on independent spatial folds via spatial cross-validation.
#' @param models Named list with models resulting from [rf()], [rf_spatial()], [rf_tuning()], or [rf_evaluate()]. Example: `models = list(a = model.a, b = model.b)`. Default: `NULL`
#' @param xy Data frame or matrix with two columns containing coordinates and named "x" and "y". Default: `NULL`
#' @param repetitions Integer, number of spatial folds to use during cross-validation. Must be lower than the total number of rows available in the model's data. Default: `30`
#' @param training.fraction Proportion between 0.5 and 0.9 indicating the proportion of records to be used as training set during spatial cross-validation. Default: `0.75`
#' @param distance.step (optional; numeric) Numeric vector of length one or two. Distance step used during the growth of the buffer containing the training cases. Must be in the same units as the coordinates in `xy`. When only one distance is provided, the same growth is applied to the x and y axes. If two distances are provided, the first one is applied to the x axis, and the second one to the y. When `NULL`, it uses 1/1000th of the range of each axis as distance. The smaller this number is, the easier is to achieve an accurate `training.fraction`, but the slower the algorithm becomes. Default: `NULL`
#' @param fill.color Character vector with hexadecimal codes (e.g. "#440154FF" "#21908CFF" "#FDE725FF"), or function generating a palette (e.g. `viridis::viridis(100)`). Default: `viridis::viridis(100, option = "F", direction = -1)`
#' @param line.color Character string, color of the line produced by `ggplot2::geom_smooth()`. Default: `"gray30"`
#' @param seed Integer, random seed to facilitate reproduciblity. If set to a given number, the results of the function are always the same. Default: `1`.
#' @param verbose Logical. If `TRUE`, messages and plots generated during the execution of the function are displayed, Default: `TRUE`
#' @param n.cores Integer, number of cores used by \code{\link[ranger]{ranger}} for parallel execution (used as value for the argument `num.threads` in `ranger()`). Default: `NULL`
#' @param cluster A cluster definition generated with `parallel::makeCluster()` or \code{\link{start_cluster}}. Only advisable if you need to spread a large number of repetitions over the nodes of a large cluster. If provided, overrides `n.cores`. The function does not stop a cluster, please remember to shut it down with `parallel::stopCluster(cl = cluster_name)` at the end of your pipeline. Default: `parallel::detectCores() - 1`
#' @return A list with three slots:
#' \itemize{
#' \item `comparison.df`: Data frame with one performance value per spatial fold, metric, and model.
#' \item `spatial.folds`: List with the indices of the training and testing records for each evaluation repetition.
#' \item `plot`: Violin-plot of `comparison.df`.
#' }
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
#'  #fitting random forest model
#'  rf.model <- rf(
#'    data = ecoregions_df,
#'    dependent.variable.name = ecoregions_dependent_variable_name,
#'    predictor.variable.names = ecoregions_predictor_variable_names,
#'    distance.matrix = ecoregions_distance_matrix,
#'    distance.thresholds = 0,
#'    n.cores = 1
#'  )
#'
#'  #fitting a spatial model with Moran's Eigenvector Maps
#'  rf.spatial <- rf_spatial(
#'  model = rf.model,
#'  n.cores = 1
#'  )
#'
#'  #comparing the spatial and non spatial models
#'  comparison <- rf_compare(
#'  models = list(
#'    `Non spatial` = rf.model,
#'    Spatial = rf.spatial
#'  ),
#'  xy = ecoregions_df[, c("x", "y")],
#'  n.cores = 1
#'  )
#'
#' }
#' @seealso [rf_evaluate()]
#' @rdname rf_compare
#' @export
rf_compare <- function(
  models = NULL,
  xy = NULL,
  repetitions = 30,
  training.fraction = 0.75,
  distance.step = NULL,
  fill.color = viridis::viridis(
    100,
    option = "F",
    direction = -1,
    alpha = 0.8
  ),
  line.color = "gray30",
  seed = 1,
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
    choices = c("r.squared", "rmse", "nrmse", "auc"),
    several.ok = TRUE
  )

  #missing input
  if(is.null(models)){
    stop("There are no models to compare in 'models'.")
  }
  if(is.null(xy)){

    #get xy of all models
    xy.models <- lapply(
      lapply(
        models,
        "[[",
        "ranger.arguments"
      ),
      "[[",
      "xy"
    )

    #compute means
    xy.means <- lapply(
      xy.models,
      FUN = function(x) mean(as.matrix(x))
    ) %>%
      unlist() %>%
      na.omit()

    #if they are equal, use xy of the first model as xy
    if(
      all(
        abs(xy.means - mean(xy.means)) < .Machine$double.eps ^ 0.5
      )
    ){
      xy <- models[[1]]$ranger.arguments$xy
    } else {
      stop("Argument 'xy' is missing.")
    }

  }

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
      seed = seed,
      verbose = FALSE,
      n.cores = n.cores,
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
    ) %>%
    na.omit()

  #df to plot
  x <- evaluation.df
  x[x$metric == "r.squared", "metric"] <- "R squared"
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

  out.list

}
