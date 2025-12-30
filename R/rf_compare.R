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
#' @param fill.color Character vector with hexadecimal codes (e.g. "#440154FF" "#21908CFF" "#FDE725FF"), or function generating a palette (e.g. `grDevices::hcl.colors(100)`). Default: `grDevices::hcl.colors(100, palette = "Zissou 1", rev = FALSE)`
#' @param line.color Character string, color of the line produced by `ggplot2::geom_smooth()`. Default: `"gray30"`
#' @param seed Integer, random seed to facilitate reproduciblity. If set to a given number, the results of the function are always the same. Default: `1`.
#' @param verbose Logical. If `TRUE`, messages and plots generated during the execution of the function are displayed, Default: `TRUE`
#' @param n.cores Integer. Number of threads for ranger's internal parallelization. Default: `NULL` (auto-detected: if user has set a parallel plan via `future::plan()`, defaults to 1; otherwise defaults to `future::availableCores(omit = 1)`). Set to 1 for debugging. Note: When using a parallel plan (multiple workers), setting n.cores > 1 may cause oversubscription. The function will warn if this occurs.
#' @return A list with three slots:
#' \itemize{
#' \item `comparison.df`: Data frame with one performance value per spatial fold, metric, and model.
#' \item `spatial.folds`: List with the indices of the training and testing records for each evaluation repetition.
#' \item `plot`: Violin-plot of `comparison.df`.
#' }
#' @examples
#'
#' if(interactive()){
#'
#'   data(
#'     plants_rf,
#'     plants_rf_spatial,
#'     plants_xy
#'   )
#'
#'   comparison <- rf_compare(
#'     models = list(
#'       `Non spatial` = plants_rf,
#'       Spatial = plants_rf_spatial
#'     ),
#'     repetitions = 5,
#'     xy = plants_xy,
#'     metrics = "rmse",
#'     n.cores = 1
#'   )
#'
#' }
#'
#' @seealso [rf_evaluate()]
#' @rdname rf_compare
#' @family model_workflow
#' @export
#' @autoglobal
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
  fill.color = grDevices::hcl.colors(
    n = 100,
    palette = "Zissou 1",
    rev = FALSE,
    alpha = 0.8
  ),
  line.color = "gray30",
  seed = 1,
  verbose = TRUE,
  n.cores = NULL
) {
  #capturing user options
  user.options <- options()
  #avoid dplyr messages
  options(dplyr.summarise.inform = FALSE)
  on.exit(options(user.options))

  #testing method argument
  metrics <- match.arg(
    arg = metrics,
    choices = c("r.squared", "pseudo.r.squared", "rmse", "nrmse", "auc"),
    several.ok = TRUE
  )

  #missing input
  if (is.null(models)) {
    stop("There are no models to compare in 'models'.")
  }
  if (is.null(xy)) {
    stop("Case coordinates 'xy' is missing.")
  }

  #list to store evaluation outputs
  evaluation.list <- list()

  #iterating through models
  for (model.i in names(models)) {
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
      n.cores = n.cores
    )

    #getting evaluation data frame
    evaluation.df.i <- models[[model.i]]$evaluation$per.fold.long
    evaluation.df.i$model.name <- model.i

    #adding it to the evaluation list
    evaluation.list[[model.i]] <- evaluation.df.i
  } #end of loop

  #binding data frames
  evaluation.df <- do.call("rbind", evaluation.list)
  rownames(evaluation.df) <- NULL

  #remove non testing models
  evaluation.df <- evaluation.df[
    evaluation.df$model == "Testing",
    c("metric", "value", "model.name")
  ]
  colnames(evaluation.df)[colnames(evaluation.df) == "model.name"] <- "model"

  evaluation.df <- stats::na.omit(evaluation.df) |>
    as.data.frame()

  #df to plot
  x <- evaluation.df
  x[x$metric == "r.squared", "metric"] <- "R squared"
  x[x$metric == "pseudo.r.squared", "metric"] <- "pseudo R squared"
  x[x$metric == "rmse", "metric"] <- "RMSE"
  x[x$metric == "nrmse", "metric"] <- "NRMSE"
  x[x$metric == "auc", "metric"] <- "AUC"

  n.models <- length(models)
  if (length(fill.color) != 1) {
    if (length(fill.color) > length(n.models)) {
      fill.colors.function <- grDevices::colorRampPalette(
        fill.color,
        alpha = TRUE
      )
      fill.color <- fill.colors.function(n.models)
    }
  }

  use.notch <- FALSE
  if (nrow(x > 30)) {
    use.notch <- TRUE
  }

  #plot
  p <- ggplot2::ggplot(data = x) +
    ggplot2::aes(
      group = model,
      y = stats::reorder(
        model,
        value,
        FUN = stats::median
      ),
      x = value,
      fill = stats::reorder(
        model,
        value,
        FUN = stats::median
      )
    ) +
    ggplot2::geom_violin(
      color = line.color
    ) +
    ggplot2::geom_boxplot(
      notch = use.notch,
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
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  #out list
  out.list <- list()
  out.list$comparison.df <- evaluation.df
  out.list$spatial.folds <- models[[model.i]]$evaluation$spatial.folds
  out.list$plot <- p

  if (verbose) {
    suppressMessages(print(p))
  }

  out.list
}
