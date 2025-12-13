#' @title Plots a tuning object produced by [rf_tuning()]
#' @description Plots the tuning of the hyperparameters `num.trees`, `mtry`, and `min.node.size` performed by [rf_tuning()].
#' @usage
#' plot_tuning(
#'   model,
#'   point.color = viridis::viridis(
#'     100,
#'     option = "F"
#'   ),
#'   verbose = TRUE
#' )
#' @param model A model fitted with [rf_tuning()]. Default: `NULL`
#' @param point.color Colors of the plotted points. Can be a single color name (e.g. "red4"), a character vector with hexadecimal codes (e.g. "#440154FF" "#21908CFF" "#FDE725FF"), or function generating a palette (e.g. `viridis::viridis(100)`). Default: `viridis::viridis(100, option = "F")`
#' @param verbose Logical, if `TRUE`, the plot is printed. Default: `TRUE`
#' @return A ggplot.
#' @seealso [rf_tuning()]
#' @examples
#'
#' #load example data
#' data(plants_df)
#'
#' #fit random forest model
#' rf.model <- rf(
#'   data = plants_df,
#'   dependent.variable.name = plants_response,
#'   predictor.variable.names = plants_predictors,
#'   distance.matrix = plants_distance,
#'   distance.thresholds = 0,
#'   n.cores = 1,
#'   verbose = FALSE
#' )
#'
#' #tune random forest model
#' rf.model <- rf_tuning(
#'  model = rf.model,
#'  xy = plants_df[, c("x", "y")],
#'  n.cores = 1,
#'  verbose = FALSE
#' )
#'
#' #generate tuning plot
#' plot_tuning(model = rf.model)
#'
#' @rdname plot_tuning
#' @export
plot_tuning <- function(
  model,
  point.color = viridis::viridis(
    100,
    option = "F"
  ),
  verbose = TRUE
) {
  #declaring variables
  metric <- NULL
  value <- NULL

  if (!("tuning" %in% names(model))) {
    stop("Object 'x' does not have a 'tuning' slot.")
  }

  #extracting tuning data frame
  tuning <- model$tuning$tuning.df

  #getting metric name
  metric.name <- model$tuning$metric

  #changing name of metric column
  colnames(tuning)[colnames(tuning) == metric.name] <- "metric"

  #to long format
  tuning.long <- tidyr::pivot_longer(
    tuning,
    cols = 1:3,
    names_to = "parameter",
    values_to = "value"
  ) %>%
    as.data.frame()

  #tuning plot
  p <- ggplot2::ggplot(
    data = tuning.long,
    ggplot2::aes(
      y = metric,
      x = value,
      fill = metric
    )
  ) +
    ggplot2::geom_smooth(
      se = TRUE,
      method = "loess",
      color = "gray20",
      alpha = 0.5,
      formula = y ~ x
    ) +
    ggplot2::geom_point(
      shape = 21,
      alpha = 0.75,
      size = 3
    ) +
    ggplot2::facet_wrap(
      "parameter",
      ncol = 1,
      scales = "free"
    ) +
    ggplot2::scale_fill_gradientn(colors = point.color) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::xlab("") +
    ggplot2::ylab(metric.name) +
    ggplot2::ggtitle("Model tuning via spatial cross-validation") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  if (verbose == TRUE) {
    suppressWarnings(suppressMessages(print(p)))
  }

  return(p)
}
